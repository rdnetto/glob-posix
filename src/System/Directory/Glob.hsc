{-# LANGUAGE ForeignFunctionInterface #-}

module System.Directory.Glob (
    glob,
    globMany,
    globAppend,
    globBrace,
    globMark,
    globNoCheck,
    globNoEscape,
    globNoMagic,
    globNoSort,
    globOnlyDir,
    globPeriod,
    globTilde,
    globTildeCheck
    ) where

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


-- Each call to glob() involves String marshalling and return code checking
c_glob' :: Ptr CGlob -> CInt -> String -> IO ()
c_glob' globPtr f p = withCString p $ \p' -> do
    errCode <- c_glob p' f nullPtr globPtr

    unless (errCode == 0 || errCode == (#const GLOB_NOMATCH))
        (error $ "glob failed with exit code: " ++ show errCode)

-- Finds pathnames matching a pattern.
glob :: [GlobFlag] -> String -> IO [FilePath]
glob flags pat = alloca $ \globPtr -> do
    -- Call glob
    c_glob' globPtr (orFlags flags) pat

    -- Unpack results
    CGlob strs <- peek globPtr
    res <- mapM peekCString strs

    -- Cleanup
    c_globfree globPtr
    return res

-- Like glob, but for multiple patterns.
-- This function only marshals data once, making it more efficient than multiple glob calls.
globMany :: [GlobFlag] -> [String] -> IO [FilePath]
globMany _ [] = return []       -- Need to handle this explicitly, or we'll free an uninitiallized glob_t.
globMany flags (p0:ps) =
    let flags' = orFlags flags
    in  alloca $ \globPtr -> do
            -- First call to glob must be *without* GLOB_APPEND
            let flags' = orFlags flags
            c_glob' globPtr flags' p0

            -- Call glob for the remaining patterns with GLOB_APPEND
            forM_ ps . c_glob' globPtr $ flags' .|. unwrapFlag globAppend

            -- Unpack paths
            CGlob strs <- peek globPtr
            res <- mapM peekCString strs

            -- Cleanup
            c_globfree globPtr
            return res

-- Helper function that ORs the flags together.
orFlags :: [GlobFlag] -> CInt
orFlags = foldl' (.|.) 0 . map unwrapFlag

unwrapFlag :: GlobFlag -> CInt
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
#enum GlobFlag, GlobFlag, GLOB_APPEND
#enum GlobFlag, GlobFlag, GLOB_BRACE
#enum GlobFlag, GlobFlag, GLOB_MARK
#enum GlobFlag, GlobFlag, globNoCheck  = GLOB_NOCHECK
#enum GlobFlag, GlobFlag, globNoEscape = GLOB_NOESCAPE
#enum GlobFlag, GlobFlag, globNoMagic  = GLOB_NOMAGIC
#enum GlobFlag, GlobFlag, globNoSort   = GLOB_NOSORT
#enum GlobFlag, GlobFlag, globOnlyDir  = GLOB_ONLYDIR
#enum GlobFlag, GlobFlag, GLOB_PERIOD
#enum GlobFlag, GlobFlag, GLOB_TILDE
#enum GlobFlag, GlobFlag, GLOB_TILDE_CHECK

-- We don't use this, so its type doesn't matter
type C_ErrorFunc = Ptr ()


-- int glob(const char *pattern, int flags, int (*errfunc) (const char *epath, int eerrno), glob_t *pglob);
foreign import ccall "glob.h glob"
     c_glob :: CString -> CInt -> C_ErrorFunc -> Ptr CGlob -> IO CInt

-- void globfree(glob_t *pglob);
foreign import ccall "glob.h globfree"
     c_globfree :: Ptr CGlob -> IO ()

