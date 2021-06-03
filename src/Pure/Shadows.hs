{-# language ViewPatterns, LambdaCase #-}
module Pure.Shadows (Direction(..),shadow) where

import Pure.Data.Styles ((<<>>),elems,rgba,pxs,inset)
import Pure.Data.Txt (Txt)

import Prelude hiding (Left,Right)

data Direction 
  = InsetTop | InsetRight | InsetBottom | InsetLeft
  | OffsetTop | OffsetRight | OffsetBottom | OffsetLeft

shadow :: Direction -> Int -> Txt
shadow dir depth = 
  case dir of
    InsetTop     -> defaultShadowsTopInset    !! depth
    InsetRight   -> defaultShadowsRightInset  !! depth
    InsetBottom  -> defaultShadowsBottomInset !! depth
    InsetLeft    -> defaultShadowsLeftInset   !! depth
    OffsetTop    -> defaultShadowsTop         !! depth
    OffsetRight  -> defaultShadowsRight       !! depth
    OffsetBottom -> defaultShadowsBottom      !! depth
    OffsetLeft   -> defaultShadowsLeft        !! depth

isInset :: Direction -> Bool
isInset = \case
  InsetTop    -> True
  InsetRight  -> True
  InsetBottom -> True
  InsetLeft   -> True
  _           -> False

{-# NOINLINE defaultShadowsBottom #-}
defaultShadowsBottom :: [Txt]
defaultShadowsBottom = fmap (createDefaultShadow OffsetBottom) [0..]

{-# NOINLINE defaultShadowsBottomInset #-}
defaultShadowsBottomInset :: [Txt]
defaultShadowsBottomInset = fmap (createDefaultShadow InsetBottom) [0..]

{-# NOINLINE defaultShadowsTop #-}
defaultShadowsTop :: [Txt]
defaultShadowsTop = fmap (createDefaultShadow OffsetTop) [0..]

{-# NOINLINE defaultShadowsTopInset #-}
defaultShadowsTopInset :: [Txt]
defaultShadowsTopInset = fmap (createDefaultShadow InsetTop) [0..]

{-# NOINLINE defaultShadowsLeft #-}
defaultShadowsLeft :: [Txt]
defaultShadowsLeft = fmap (createDefaultShadow OffsetLeft) [0..]

{-# NOINLINE defaultShadowsLeftInset #-}
defaultShadowsLeftInset :: [Txt]
defaultShadowsLeftInset = fmap (createDefaultShadow InsetLeft) [0..]

{-# NOINLINE defaultShadowsRight #-}
defaultShadowsRight :: [Txt]
defaultShadowsRight = fmap (createDefaultShadow OffsetRight) [0..]

{-# NOINLINE defaultShadowsRightInset #-}
defaultShadowsRightInset :: [Txt]
defaultShadowsRightInset = fmap (createDefaultShadow InsetRight) [0..]

createDefaultShadow :: Direction -> Int -> Txt
createDefaultShadow dir n = createShadow dir (isInset dir) n umbra_color penumbra_color ambient_color
  where
    umbra_color = rgba(0,0,0,0.2)
    penumbra_color = rgba(0,0,0,0.14)
    ambient_color = rgba(0,0,0,0.12)

-- An approximated implementation of material shadows
createShadow :: Direction -> Bool -> Int -> Txt -> Txt -> Txt -> Txt
createShadow d i (fromIntegral -> n) umbra_color penumbra_color ambient_color = elems [umbra,penumbra,ambient]
  where
    ins | i = inset | otherwise = mempty

    dir :: Int -> Int -> Txt
    dir x y =
      case d of
        InsetTop     -> pxs x <<>> pxs (negate y)
        OffsetTop    -> pxs x <<>> pxs (negate y)
        InsetBottom  -> pxs x <<>> pxs y
        OffsetBottom -> pxs x <<>> pxs y
        InsetLeft    -> pxs (negate y) <<>> pxs x
        OffsetLeft   -> pxs (negate y) <<>> pxs x
        InsetRight   -> pxs y <<>> pxs x
        OffsetRight  -> pxs y <<>> pxs x

    umbra = dir 0 offset_y <<>> pxs blur_radius <<>> pxs spread_radius <<>> umbra_color <<>> ins
      where
        offset_y :: Int
        offset_y
          | 4 <- n    = 2
          | n < 8     = round (0.056 * n ^ 3 - 0.62 * n ^ 2 + 2.1 * n + 0.57)
          | otherwise = round (0.38 * n + 2)

        blur_radius :: Int
        blur_radius = round (0.333 * fromIntegral ambient_blur_radius)

        spread_radius :: Int
        spread_radius
          | n < 8     = round (-0.083 * n ^ 3 + 0.98 * n ^ 2 - 3.23 * n + 1.26)
          | otherwise = round (-0.25 * n - 0.8)
          
    penumbra = dir 0 offset_y <<>> pxs blur_radius <<>> pxs spread_radius <<>> penumbra_color <<>> ins
      where
        offset_y :: Int
        offset_y = round n

        blur_radius :: Int
        blur_radius
          | n < 8     = round (-0.084 * n ^ 3 + n ^ 2 - 1.7 * n + 2)
          | otherwise = round (1.75 * n - 3.80)

        spread_radius :: Int
        spread_radius = round (0.15 * n - 0.39)

    ambient = dir 0 offset_y <<>> pxs ambient_blur_radius <<>> pxs spread_radius <<>> ambient_color <<>> ins
      where
        offset_y :: Int
        offset_y = spread_radius + 1

        spread_radius :: Int
        spread_radius
          | n < 8     = round (1/7 * n)
          | otherwise = round (0.39 * n - 1.25)

    ambient_blur_radius :: Int
    ambient_blur_radius 
      | n < 8     = round (-0.167 * n ^ 3 + 1.87 * n ^ 2 - 3.25 * n + 4.86)
      | otherwise = round ((n - 1) * 2)
