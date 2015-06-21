{- This code is from AC-EasyRaster-GTK-1.1.3
   This code is copyright by Andrew Coppin
   I had to copy this, because AC-EasyRaster-GTK-1.1.3 does not build
   RSX 21.06.2015
-}


{- |
  This module provides exactly the same interface as
  "Graphics.EasyRaster.GTK", but with run-time sanity
  checks. This results in greater safety and reduced
  performance. Typically you will use this module while
  developing and debugging your application, and then
  change your import statement when you're happy that
  any bugs have been corrected.
-}

module GTKWrapperSave
    (
      -- * System initialisation
      init_system,


      -- * Types
      ER.Coord, ER.Channel,
      ER.ImageFileType (..), ER.type_string,
      ER.ImageBuffer (),

      -- * Image buffer creation
      ER.ib_new, ER.ib_load,

      -- * Image buffer output
      -- ER.ib_save,

      -- * Image buffer queries
      ER.ib_size, ER.ib_coords, ER.ib_coords2, ER.ib_valid_coords,

      -- * Pixel I/O
      ib_write_pixel, ib_read_pixel,

      -- * Image display
      ER.ib_display, ER.ib_canvas,

      -- * GTK event loop
      ER.process_event, ER.wait_event, ER.main_loop,

      -- * Low-level GTK access
      ER.ib_pixbuf

      -- * Example code
      -- | See "Graphics.EasyRaster.GTK" for examples.
    )
  where

import Control.Monad (when)
import Graphics.UI.Gtk
import qualified GTKWrapper as ER

{- |
  Initialise the GTK runtime system. This function /must/
  be called before any other functions in this module.
  (If not, your program will likely summarily crash with
  an obscure error to @stderr@.)

  Note that Gtk2hs will crash with a warning message if
  the program is compiled with the threaded RTS. (Use
  the non-paranoid version of @init_system@ to turn this
  warning off.)
-}
init_system :: IO ()
init_system = initGUI >> return ()

{- |
  Overwrite the specified pixel with the (R, G, B) colour.

  If the pixel coordinates are out of range, the 'error'
  function is called.
-}
ib_write_pixel :: ER.ImageBuffer -> (ER.Coord, ER.Coord) -> (ER.Channel, ER.Channel, ER.Channel) -> IO ()
ib_write_pixel buf (x,y) (r,g,b) = do
  when (not $ ER.ib_valid_coords buf (x,y)) $ error "ib_write_pixel: invalid coordinates."
  ER.ib_write_pixel buf (x,y) (r,g,b)

{- |
  Read the (R, G, B) colour of the specified pixel.

  If the pixel coordinates are out of range, the 'error'
  function is called.
-}
ib_read_pixel :: ER.ImageBuffer -> (ER.Coord, ER.Coord) -> IO (ER.Channel, ER.Channel, ER.Channel)
ib_read_pixel buf (x,y) = do
  when (not $ ER.ib_valid_coords buf (x,y)) $ error "ib_write_pixel: invalid coordinates."
  ER.ib_read_pixel buf (x,y)

