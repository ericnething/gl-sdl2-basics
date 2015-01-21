module Shader
       (
         compileShader
       , createProgram
       ) where

import Control.Monad (when)
import Graphics.GL
import Foreign
import Foreign.C
import qualified Data.ByteString as BS (readFile, useAsCString)
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
    \ptr -> with ptr $ \src -> glShaderSource shader 1 src nullPtr
  glCompileShader shader

  -- Check compile status
  status <- overPtr $ glGetShaderiv shader GL_COMPILE_STATUS
  when (status == 0) $ do
    putStrLn $ "Shader at " ++ path ++ " failed to compile."
    
    infoLogLength <- overPtr $ glGetShaderiv shader GL_INFO_LOG_LENGTH
    errors        <- allocaArray (fromIntegral infoLogLength) $ \ptr -> do
      glGetShaderInfoLog shader infoLogLength nullPtr ptr
      peekArray (fromIntegral infoLogLength) ptr

    putStrLn $ map castCCharToChar errors
  
createProgram :: Shader -> Shader -> IO Program
createProgram vertex fragment = do
  program <- glCreateProgram
  glAttachShader program vertex
  glAttachShader program fragment

  withCString "outColor" $ glBindFragDataLocation program 0

  -- Link and check
  glLinkProgram program
  status <- overPtr $ glGetProgramiv program GL_LINK_STATUS
  when (status == 0) $ do
    putStrLn $ "Program linking failed."
    
    infoLogLength <- overPtr $ glGetProgramiv program GL_INFO_LOG_LENGTH
    errors        <- allocaArray (fromIntegral infoLogLength) $ \ptr -> do
      glGetProgramInfoLog program infoLogLength nullPtr ptr
      peekArray (fromIntegral infoLogLength) ptr
  
    putStrLn $ map castCCharToChar errors

  return program
