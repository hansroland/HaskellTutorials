-- ---------------------------------------------------------------------------
-- Example from documentation of Hackage package AC-EasyRaster-GTK
-- ---------------------------------------------------------------------------
--
-- I was unable to install AC-EasyRaster-GTK from Haskell.
--   Reason: type-error in ib_save
-- Therfore I copied the code to 2 "own" modules, and deactivated ib_save
--
-- ---------------------------------------------------------------------------

module Main where

-- import Graphics.EasyRaster.GTK
import GTKWrapperSave

main = do
   init_system
   ibuf <- ib_new (640, 480)
   mapM_ (\p -> ib_write_pixel ibuf p (colour p)) (ib_coords ibuf)
   -- ib_save ibuf IFT_PNG "Example1.png"
   ib_display ibuf "Hello"
   -- Display window
   main_loop

colour :: (Coord, Coord) -> (Channel, Channel, Channel)
colour (x, y) = (floor $ fromIntegral x / 640 * 255, floor $ fromIntegral y / 480 * 255, 0)
