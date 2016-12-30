import Html exposing (beginnerProgram)

import Update
import View

main =
  beginnerProgram { model = Update.init, view = View.view, update = Update.update }
