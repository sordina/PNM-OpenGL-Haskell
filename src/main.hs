import Graphics.UI.GLUT
import System.Time
import Data.Maybe
import qualified Graphics.Rendering.OpenGL.PNM.Reader as PNM
import qualified Graphics.Rendering.OpenGL.PNM.Format as PNMF

main = do
  (_,files) <- getArgsAndInitialize
  mapM_ ((>>= renderPNM) . PNM.readFilePNM) files

renderPNM (Left  error)   = print error
renderPNM (Right pnmData) = do

  initialDisplayMode $= [DoubleBuffered]

  createWindow "PNM Viewer"

  -- left, right, bottom, top -- Pixels start at top-left
  ortho2D 0 100 100 0
  displayCallback       $= display pnmData
  keyboardMouseCallback $= Just k

  mainLoop

k (Char 'f') _ _ _ = fullscreen
k _          _ _ _ = return ()

fullscreen = do
  currentSize <- get windowSize
  screenSize <- get screenSize
  cursor $= None >> fullScreen

display values = do
    clear [ColorBuffer]
    renderPrimitive Points (image values)
    flush
    swapBuffers

image values = mapM_ f (zip (map wh [0..]) pnmData)
  where
    (w,h)   = PNMF.getResolution values
    pnmData = PNMF.getData       values
    max     = i (fromMaybe 100 . PNMF.getMax) values
    i       = (fromIntegral .)
    fi      = (/8) . fromIntegral
    wh n    = (n `mod` w, n `div` h)

    f ((x,y),(color)) = do
      rgb red green blue
      tv (fi x) (fi y)
        where
          red   = i PNMF.red   color / max
          green = i PNMF.green color / max
          blue  = i PNMF.blue  color / max

type D = GLdouble
tv :: D -> D -> IO ()
tv x y = vertex $ Vertex3 x y 0

rgb :: D -> D -> D -> IO ()
rgb r g b = color $ Color3 r g b

{- http://edndoc.esri.com/arcobjects/9.2/net/45c93c25-2ddb-4e1b-9bef-37c40b931597.htm
  void DrawMethod(…)
  {
    if (m_bOnce)
    {
       CreateDisplayLists();
       Bitmap bitmap = new Bitmap(m_imagePath);
       m_textureId = CreateTexture(bitmap);

       m_bOnce = false;
    }

     //Draw the element.
    GL.glPushMatrix();
    GL.glTranslatef((float)m_deviceFrame.left + 70.0f, (float)m_deviceFrame.top + 70.0f, 0.0f);
    GL.glScalef(scale, scale, 0.0f);
    GL.glRotatef((float)Display.DisplayTransformation.Rotation, 0.0f, 0.0f, 1.0f);
    GL.glBindTexture(GL.GL_TEXTURE_2D, (uint)m_textureId);
    GL.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    GL.glCallList(m_itemList);
    GL.glPopMatrix();
  }
-}
