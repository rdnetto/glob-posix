{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module System.Directory.Glob (glob) where

import Foreign (alloca, peekArray)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr(..), nullPtr)
import Foreign.Storable (Storable(..))

#include <glob.h>


glob :: String -> IO [FilePath]
glob pat = withCString pat $ \pat' ->
    alloca $ \globPtr -> do
        errCode <- c_glob pat' 0 nullPtr globPtr

        res <- if errCode == 0 || errCode == (#const GLOB_NOMATCH)
                  then do
                      CGlob strs <- peek globPtr
                      mapM peekCString strs
                  else error $ "glob failed with exit code: " ++ show errCode

        c_globfree globPtr
        return res


{-
    typedef struct {
        size_t   gl_pathc;    /* Count of paths matched so far  */
        char   **gl_pathv;    /* List of matched pathnames.     */
        size_t   gl_offs;     /* Slots to reserve in gl_pathv.  */
    } glob_t;
-}
newtype CGlob = CGlob [CString]

instance Storable CGlob where
    sizeOf _    = #size glob_t
    alignment _ = #alignment glob_t
    peek ptr = do
        pathC <- (#peek glob_t, gl_pathc) ptr
        pathV <- peekArray pathC  =<< (#peek glob_t, gl_pathv) ptr
        return $ CGlob pathV

    poke _ _ = error "Poke unsupported for CGlob"

-- We don't use this, so its type doesn't matter
type C_ErrorFunc = Ptr ()


-- int glob(const char *pattern, int flags, int (*errfunc) (const char *epath, int eerrno), glob_t *pglob);
foreign import ccall "glob.h glob"
     c_glob :: CString -> CInt -> C_ErrorFunc -> Ptr CGlob -> IO CInt

-- void globfree(glob_t *pglob);
foreign import ccall "glob.h globfree"
     c_globfree :: Ptr CGlob -> IO ()

