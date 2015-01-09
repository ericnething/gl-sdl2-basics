module Shader
       (
         compileShader
       , createProgram
       ) where

import Control.Monad (when)
import Graphics.GL
import Foreign
import Foreign.C
import Data.ByteString as BS (readFile, useAsCString)
import Data.ByteString.Char8 as BS (pack)
import Util
import Data.Char (chr)

type Shader = GLuint
type Program = GLuint

compileShader :: Shader -> FilePath -> IO ()
compileShader shader path = do

  -- Read the source and compile the shader
  source <- BS.readFile path
  BS.useAsCString source $
    \ptr -> withArray [ptr] $ \src -> glShaderSource shader 1 src nullPtr
  glCompileShader shader

  -- Check compile status
  status <- overPtr $ glGetShaderiv shader GL_COMPILE_STATUS
  when (status == 0) $
    putStrLn $ "Shader at " ++ path ++ " failed to compile."

  errors <- allocaArray 512 $ \ptr -> do
    glGetShaderInfoLog shader 512 nullPtr ptr
    peekArray 512 ptr
  putStrLn $ "Errors [" ++ path ++ "]:"
  print (fmap (chr . fromIntegral) errors)
  
createProgram :: Shader -> Shader -> IO Program
createProgram vertex fragment = do
  program <- glCreateProgram
  glAttachShader program vertex
  glAttachShader program fragment

  withCString "outColor" $ glBindFragDataLocation program 0

  -- Link and check
  glLinkProgram program
  status <- overPtr $ glGetProgramiv program GL_LINK_STATUS
  print status
  infoLogLength <- overPtr $ glGetProgramiv program GL_INFO_LOG_LENGTH
  errors        <- allocaArray (fromIntegral infoLogLength) $ \ptr -> do
    glGetProgramInfoLog program infoLogLength nullPtr ptr
    peekArray (fromIntegral infoLogLength) ptr
  
  putStrLn "linker errors:"
  print (fmap (chr . fromIntegral) errors)

  return program
