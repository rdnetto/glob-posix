module System.Directory.Glob.Internal where

import Foreign.C.Types (CInt(..))
#include <glob.h>


-- POSIX Control flags
data GlobFlag = GlobFlag CInt
#enum GlobFlag, GlobFlag, GLOB_APPEND
#enum GlobFlag, GlobFlag, GLOB_MARK
#enum GlobFlag, GlobFlag, globNoCheck  = GLOB_NOCHECK
#enum GlobFlag, GlobFlag, globNoEscape = GLOB_NOESCAPE
#enum GlobFlag, GlobFlag, globNoSort   = GLOB_NOSORT

-- GNU extensions
#ifdef linux_HOST_OS

#enum GlobFlag, GlobFlag, GLOB_BRACE
#enum GlobFlag, GlobFlag, globNoMagic  = GLOB_NOMAGIC
#enum GlobFlag, GlobFlag, globOnlyDir  = GLOB_ONLYDIR
#enum GlobFlag, GlobFlag, GLOB_PERIOD
#enum GlobFlag, GlobFlag, GLOB_TILDE
#enum GlobFlag, GlobFlag, GLOB_TILDE_CHECK

#else

globBrace, globNoMagic, globOnlyDir, globPeriod, globTilde, globTildeCheck :: GlobFlag
globBrace      = error "Unsupported: GLOB_BRACE is a GNU extension"
globNoMagic    = error "Unsupported: GLOB_NOMAGIC is a GNU extension"
globOnlyDir    = error "Unsupported: GLOB_ONLYDIR is a GNU extension"
globPeriod     = error "Unsupported: GLOB_PERIOD is a GNU extension"
globTilde      = error "Unsupported: GLOB_TILDE is a GNU extension"
globTildeCheck = error "Unsupported: GLOB_TILDE_CHECK is a GNU extension"

#endif

