-- -------------------------------------------------------------
-- clay21.hs - The clay code to example 2.1
-- -------------------------------------------------------------
--
-- use: renderToCssFile "berg21.css" clay21
--

import Data.Monoid
import ClayExt

clay21 = 
    body ?
      do background    black
         color         white
         border        dashed (px 2) yellow
