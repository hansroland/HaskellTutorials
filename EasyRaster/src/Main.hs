-- ---------------------------------------------------------------------------
-- Example from documentation of Hackage package AC-EasyRaster-GTK
-- ---------------------------------------------------------------------------
--
-- I was unable to install AC-EasyRaster-GTK from Haskell. 
--   Reason: type-error in ib_save
-- I copied the code to 2 "own" modules, deactivated ib_save,
--   changed the example from ib_save to ib_display but id does not display
--   any window...
--
--  Wait, until a new version shows up on Hackage...
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

colour :: (Coord, Coord) -> (Channel, Channel, Channel)
colour (x, y) = (floor $ fromIntegral x / 640 * 255, floor $ fromIntegral y / 480 * 255, 0)
