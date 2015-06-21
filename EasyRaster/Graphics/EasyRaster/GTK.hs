{- This code is from AC-EasyRaster-GTK-1.1.3
   This code is copyright by Andrew Coppin
   I had to copy this, because AC-EasyRaster-GTK-1.1.3 does not build
   RSX 21.06.2015
-}

{- |
  This module provides pixel-oriented graphics operations.
  It defines an 'ImageBuffer' type, which is a thin layer
  over a GTK+ 'Pixbuf'. It then provides functions for
  doing pixel-oriented graphics on this without having to
  do a lot of bit-twiddling math by hand. It also
  provides for quickly and easily loading and saving
  images, and displaying them on the screen.

  In the simplest case, you can import this module alone
  and never need to touch the actual GTK modules;
  however, this module provides access to the underlying
  GTK resources as well, in case you want to use it as
  part of a larger GTK program.

  Note that /displaying/ an image on screen requires
  you to run the GTK event loop. If you only want to
  load, save and process image files, you can completely
  ignore the GTK event loop. (You do still need to call
  'init_system', however.)
-}

module Graphics.EasyRaster.GTK
    (
      -- * System initialisation
      init_system,

      -- * Types
      Coord, Channel,
      ImageFileType (..), type_string,
      ImageBuffer (),

      -- * Image buffer creation
      ib_new, ib_load,

      -- * Image buffer output
      -- ib_save,

      -- * Image buffer queries
      ib_size, ib_coords, ib_coords2, ib_valid_coords,

      -- * Pixel I/O
      ib_write_pixel, ib_read_pixel,

      -- * Image display
      ib_display, ib_canvas,

      -- * GTK event loop
      process_event, wait_event, main_loop,

      -- * Low-level GTK access
      ib_pixbuf

      -- * Example code

      {- |
        The following short program should give you an idea
        of how to use this library. It draws a simple graphic
        and saves it to disk.

        > module Main where
        >
        > import Graphics.EasyRaster.GTK
        >
        > main = do
        >   init_system
        >   ibuf <- ib_new (640, 480)
        >   mapM_ (\p -> ib_write_pixel ibuf p (colour p)) (ib_coords ibuf)
        >   ib_save ibuf IFT_PNG "Example1.png"
        >
        > colour :: (Coord, Coord) -> (Channel, Channel, Channel)
        > colour (x, y) = (floor $ fromIntegral x / 640 * 255, floor $ fromIntegral y / 480 * 255, 0)
      -}
    )
  where

import Data.Word
import Data.Array.IO
import Data.Array.Base (unsafeWrite, unsafeRead)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC -- Necessary with gtk-0.11.2 for some reason.

{- |
  Initialise the GTK runtime system. This function /must/
  be called before any other functions in this module.
  (If not, your program will likely summarily crash with
  an obscure error to @stderr@.)

  Note that Gtk2hs by default will crash with a warning
  message if the program is compiled with the threaded
  RTS; this module disables that warning. However, be
  warned: You must /only/ call the GTK-based functions
  from thread 0.
-}
init_system :: IO ()
init_system = unsafeInitGUIForThreadedRTS >> return ()

{- |
  Process one GTK event if there are any waiting, otherwise
  do nothing.
-}
process_event :: IO ()
process_event = mainIterationDo False >> return ()

{- |
  Process one GTK event. Wait for an event to arrive if
  necessary.
-}
wait_event :: IO ()
wait_event = mainIterationDo True >> return ()

{- |
  Run the GTK \"main loop\" until GTK's 'mainQuit'
  function is called. (Note that this means that
  thread 0 can do no other work while the main
  loop is running.)
-}
main_loop :: IO ()
main_loop = mainGUI

-- | The type of pixel coordinates.
type Coord   = Int

-- | The type of pixel channels.
type Channel = Word8

{- |
  An @ImageBuffer@ is a space-efficient grid of 24-bit
  RGB pixels (i.e., 8 bits per channel) that the GTK
  library can also access. (The latter gives us the
  ability to load\/save\/display the graphics, rather
  than just manipulate it in memory.)
-}
data ImageBuffer = ImageBuffer
  {
    pixbuf :: Pixbuf,
    sizeX, sizeY :: Coord,
    rowstride :: Coord,
    raw :: PixbufData Coord Channel
  }

{- |
  Used to indicate image file type.
-}
data ImageFileType =
    IFT_PNG | -- ^ PNG image.
    IFT_JPEG  -- ^ JPEG image.
  deriving (Eq, Enum, Bounded, Read, Show)

{- |
  Convert an 'ImageFileType' to a plain string (all
  lower-case).
-}
type_string :: ImageFileType -> String
type_string IFT_PNG  = "png"
type_string IFT_JPEG = "jpeg"

{- |
  Make a new 'ImageBuffer' of the specified size. Valid
  coordinates will be in the range (0,0) to (x-1, y-1),
  with (0,0) being the top-left corner of the image.
-}
ib_new :: (Int,Int) -> IO ImageBuffer
ib_new (x,y) = do
  core <- pixbufNew ColorspaceRgb False 8 x y
  build core

{- |
  Load a graphics file into a newly created 'ImageBuffer'.
  The file type is determined automatically (any type that
  GTK+ supports), as is the size for the 'ImageBuffer'.
  (This can be queried later.)
-}
ib_load :: FilePath -> IO ImageBuffer
ib_load f = do
  core <- pixbufNewFromFile f
  build core

{- |
  Save an 'ImageBuffer' into a file with the specified
  file type. Note that the filename should include an
  appropriate suffix (e.g., @.png@). The type of file is
  determined by the 'ImageFileType' argument, /not/ by
  the filename.
-}

{- deactivated by RSX
ib_save :: ImageBuffer -> ImageFileType -> FilePath -> IO ()
ib_save buf t f = do
  pixbufSave (pixbuf buf) f (type_string t) []
  return ()
-}

{- |
  Returns a GTK+ canvas object which displays the graphcs
  in the 'ImageBuffer' and will automatically repaint
  itself in response to damage.
-}
ib_canvas :: ImageBuffer -> IO DrawingArea
ib_canvas ib = do
    canvas <- drawingAreaNew
    canvas `onExposeRect` (redraw canvas)
    return canvas
  where
    redraw canvas _ = do
      dc <- widgetGetDrawWindow canvas
      gc <- gcNew dc
      drawPixbuf dc gc (pixbuf ib) 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
      return ()

{- |
  This is a quick shortcut for displaying an image
  on screen. Given an 'ImageBuffer' and a window
  title, this will display the image in a new window.
  Clicking the window's close button will call GTK's
  'mainQuit' function. For example,

  > ib_display buf "My Window Title"
  > main_loop

  will display @buf@ on screen and halt the program
  (or at least thread 0) until the user clicks the
  close button. Crude, but useful for quick testing.
-}
ib_display :: ImageBuffer -> String -> IO ()
ib_display ib title = do
  window <- windowNew
  windowSetTitle window title
  windowSetDefaultSize window (sizeX ib) (sizeY ib)
  canvas <- ib_canvas ib
  containerAdd window canvas
  window `onDestroy` mainQuit
  widgetShowAll window

build :: Pixbuf -> IO ImageBuffer
build core = do
  sx <- pixbufGetWidth     core
  sy <- pixbufGetHeight    core
  s  <- pixbufGetRowstride core
  r  <- pixbufGetPixels    core
  return $ ImageBuffer
    {
      pixbuf = core,
      sizeX = sx,
      sizeY = sy,
      rowstride = s,
      raw = r
    }

{- |
  Return the underlying GTK 'Pixbuf' used by an 'ImageBuffer'.
  (In case you want to do something to it with GTK.)
-}
ib_pixbuf :: ImageBuffer -> Pixbuf
ib_pixbuf = pixbuf

{- |
  Query the size of an 'ImageBuffer'. This returns the width
  and height in pixels. Valid coordinates for this @ImageBuffer@
  are from (0,0) to (x-1, y-1), where (x,y) is the value returned
  from @ib_size@.

  Note that this is a /pure/ function (since the size of an
  'ImageBuffer' is immutable).
-}
ib_size :: ImageBuffer -> (Coord,Coord)
ib_size buf = (sizeX buf, sizeY buf)

{- |
  Determine whether the specified pixel coordinates are valid
  for this 'ImageBuffer' (i.e., check they're not off the
  edge of the image).
-}
ib_valid_coords :: ImageBuffer -> (Coord,Coord) -> Bool
ib_valid_coords buf (x,y) = x < sizeX buf && y < sizeY buf

{- |
  Return a list containing all valid coordinates for this 'ImageBuffer'.
  Useful when you want to process all pixels in the image; now you
  can just use 'mapM' or whatever.
-}
ib_coords :: ImageBuffer -> [(Coord,Coord)]
ib_coords buf = [ (x,y) | y <- [0 .. (sizeY buf) - 1], x <- [0 .. (sizeX buf) - 1] ]

{- |
  Return all valid coordinates as a list of lists instead of a single
  flat list. (In case you need to do something at the end of each row.)
-}
ib_coords2 :: ImageBuffer -> [[(Coord,Coord)]]
ib_coords2 buf = [ [ (x,y) | x <- [0 .. (sizeX buf) - 1] ] | y <- [0 .. (sizeY buf) - 1] ]

{- |
  Overwrite the specified pixel with the specified colour (R, G, B).

  Note that pixel coordinates are /not/ checked, for efficiency reasons.
  However, supplying invalid pixel coordinates may result in behaviour ranging
  from corrupted graphical output to random program crashes. See
  "Graphics.EasyRaster.GTK.Paranoid" for a version of this function with
  bounds checking turned on.
-}
ib_write_pixel :: ImageBuffer -> (Coord,Coord) -> (Channel,Channel,Channel) -> IO ()
ib_write_pixel buf (x,y) (r,g,b) = do
  -- Caution: No range checks!!
  let p = y * rowstride buf + x * 3
  unsafeWrite (raw buf) (p + 0) r
  unsafeWrite (raw buf) (p + 1) g
  unsafeWrite (raw buf) (p + 2) b

{- |
  Read the current (R, G, B) colour valid of the specified pixel.

  Note that pixel coordinates are /not/ checked, for efficiency reasons.
  However, supplying invalid pixel coordinates will at best result in
  gibberish being returned, and at worst a segmentation fault. See
  "Graphics.EasyRaster.Paranoid" for a version of this function with
  bounds checking turned on.
-}
ib_read_pixel :: ImageBuffer -> (Coord,Coord) -> IO (Channel,Channel,Channel)
ib_read_pixel buf (x,y) = do
  -- Cation: No range checks!!
  let p = y * rowstride buf + x * 3
  r <- unsafeRead (raw buf) (p + 0)
  g <- unsafeRead (raw buf) (p + 1)
  b <- unsafeRead (raw buf) (p + 2)
  return (r,g,b)
