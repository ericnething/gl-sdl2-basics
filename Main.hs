{-# LANGUAGE OverloadedStrings, PatternSynonyms, StandaloneDeriving #-}

module Main where

import Graphics.GL
import qualified Graphics.UI.SDL as SDL
import Foreign
import Foreign.C
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.ByteString as BS (readFile, useAsCString)
import Data.Char (chr)

import Shader
import Util

deriving instance Ord SDL.Keysym

main :: IO ()
main = do
  let createWindow t =
        SDL.createWindow t 0 0 800 600 SDL.SDL_WINDOW_SHOWN
      title = "Eric's Dragon Quest"

  SDL.init SDL.SDL_INIT_EVERYTHING
  window <- withCString title createWindow
  
  -- screenSurface <- SDL.getWindowSurface window
  -- pixelFormat <- peek screenSurface
  -- color <- SDL.mapRGB (SDL.surfaceFormat pixelFormat) 0xFF 0xFF 0xFF
  -- SDL.fillRect screenSurface nullPtr color
  -- SDL.updateWindowSurface window

  SDL.glSetAttribute SDL.SDL_GL_CONTEXT_PROFILE_MASK SDL.SDL_GL_CONTEXT_PROFILE_CORE
  SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION 3
  SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION 2

  SDL.glSetAttribute SDL.SDL_GL_DOUBLEBUFFER 1
  SDL.glSetAttribute SDL.SDL_GL_DEPTH_SIZE 24

  renderer <- SDL.glCreateContext window
  
  -- SDL.delay 5000

  initResources
  loop window Set.empty

  SDL.glDeleteContext renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Window -> Set SDL.Keysym -> IO ()
loop window keys = do
  keys' <- parseEvents keys

  -- Draw
  glClearColor 0.0 0.0 0.0 1.0
  glClear GL_COLOR_BUFFER_BIT
  glDrawArrays GL_TRIANGLES 0 3
  
  SDL.glSwapWindow window
  
  unless (Set.member escapeKey keys') $
    loop window keys'

escapeKey :: SDL.Keysym
escapeKey = SDL.Keysym SDL.SDL_SCANCODE_ESCAPE SDL.SDLK_ESCAPE SDL.KMOD_NONE

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keys = do
  
  let pollEvent = alloca $ \ptr ->
        do r <- SDL.pollEvent ptr
           case r of
            0 -> return Nothing
            _ -> maybePeek peek ptr
            
  mevent <- pollEvent

  case mevent of
   Nothing -> return keys
   Just event ->

     case event of

      SDL.KeyboardEvent SDL.SDL_KEYUP _ _ _ _ k ->
        parseEvents (Set.insert k keys)

      SDL.KeyboardEvent SDL.SDL_KEYDOWN _ _ _ _ k ->
        parseEvents (Set.delete k keys)

      SDL.QuitEvent{} ->
        parseEvents (Set.insert escapeKey keys)

      _ -> parseEvents keys

vertices :: [GLfloat]
vertices =
  [  0.0,  0.5
  ,  0.5, -0.5
  , -0.5, -0.5
  ]

initResources = do

  -- VAO
  vao <- overPtr (glGenVertexArrays 1)
  glBindVertexArray vao
  
  -- VBO
  vbo <- overPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER vbo
  let size = fromIntegral $ length vertices * sizeOf (1.0 :: Float)
  withArray vertices $ \ptr ->
    glBufferData GL_ARRAY_BUFFER size (castPtr ptr) GL_STATIC_DRAW

  -- Vertex Shader
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  compileShader vertexShader "triangle.vertex"

  -- Fragment Shader
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  compileShader fragmentShader "triangle.fragment"

  -- Shader Program
  program <- createProgram vertexShader fragmentShader
  glUseProgram program

  -- Link Vertex data with Attributes
  posAttrib <- withCString "position" $ glGetAttribLocation program
  glVertexAttribPointer (fromIntegral posAttrib) 2 GL_FLOAT GL_FALSE 0 nullPtr
  glEnableVertexAttribArray (fromIntegral posAttrib)

  -- Uniforms
  uniColor <- withCString "triangleColor" $ glGetUniformLocation program
  glUniform3f uniColor 1.0 0.0 0.0
