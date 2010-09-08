import Graphics.UI.GLUT

main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  -- depthFunc       $= Just Less

  createWindow "PNM Viewer"

  displayCallback $= display
  -- idleCallback $= Just display

  mainLoop

display = do
    clear [ColorBuffer]

    {- For 2D display at full scale, taken from http://basic4gl.wikispaces.com/2D+Drawing+in+OpenGL
    const XSize = 640, YSize = 480
    glMatrixMode (GL_PROJECTION)
    glLoadIdentity ();
    glOrtho (0, XSize, YSize, 0, 0, 1)
    glMatrixMode (GL_MODELVIEW)
    -}

    renderPrimitive Points grad

    flush
    swapBuffers

grad = mapM_ f points
  where
    f (r,g) = do
      rgb r g 1
      tv r g 0

points :: [(D,D)]
points = [(r/100,g/100) | g <- [0..100], r <- [0..100]]

type D = GLdouble
tv :: D -> D -> D -> IO ()
tv x y z = vertex $ Vertex3 x y z

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
