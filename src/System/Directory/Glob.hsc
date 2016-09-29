{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module System.Directory.Glob where

import Control.Exception (bracket)
import Control.Monad (unless, forM_)
import Data.Bits ((.|.))
import Data.List (foldl')
import Foreign (alloca, free, peekArray)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr(..), nullPtr)
import Foreign.Storable (Storable(..))

#include <glob.h>


-- Finds pathnames matching a pattern.
glob :: [GlobFlag] -> String -> IO [FilePath]
glob flags pat = withCString pat $ \pat' ->
    alloca $ \globPtr -> do
        errCode <- c_glob pat' (orFlags flags) nullPtr globPtr

        res <- if errCode == 0 || errCode == (#const GLOB_NOMATCH)
                  then do
                      CGlob strs <- peek globPtr
                      mapM peekCString strs
                  else error $ "glob failed with exit code: " ++ show errCode

        c_globfree globPtr
        return res

-- Like glob, but for multiple patterns.
-- This function only marshals data once, making it more efficient than multiple glob calls.
globMany :: [GlobFlag] -> [String] -> IO [FilePath]
globMany flags pats =
    let flags' = orFlags $ globAppend : flags
    in  alloca $ \globPtr -> do
            -- Call glob for each pattern
            forM_ pats $ \p -> withCString p $ \pat' -> do
                putStrLn p
                errCode <- c_glob pat' flags' nullPtr globPtr

                unless (errCode == 0 || errCode == (#const GLOB_NOMATCH))
                    (error $ "glob failed with exit code: " ++ show errCode)

            -- Unpack paths
            CGlob strs <- peek globPtr
            mapM peekCString strs


-- Helper function that ORs the flags together.
orFlags :: [GlobFlag] -> CInt
orFlags = foldl' (.|.) 0 . map unwrapFlag where
    unwrapFlag (GlobFlag f) = f



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

-- Control flags
data GlobFlag = GlobFlag CInt
#enum GlobFlag, GlobFlag, GLOB_MARK, GLOB_NOSORT, GLOB_APPEND, GLOB_NOESCAPE

-- We don't use this, so its type doesn't matter
type C_ErrorFunc = Ptr ()


-- int glob(const char *pattern, int flags, int (*errfunc) (const char *epath, int eerrno), glob_t *pglob);
foreign import ccall "glob.h glob"
     c_glob :: CString -> CInt -> C_ErrorFunc -> Ptr CGlob -> IO CInt

-- void globfree(glob_t *pglob);
foreign import ccall "glob.h globfree"
     c_globfree :: Ptr CGlob -> IO ()

