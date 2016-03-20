module Environments where 

import Data.Maybe

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Monoid
import Data

data Environment = Env
    { types :: TyEnv
    }

data Used = Used
    { usedTypeNames :: TName
    , usedLabel     :: Label
    , usedLabelNames  :: LName
    }

data Records = Records
    { fpCons    :: FpCons
    , tyCons    :: TyCons
    , typedProg :: TypedProg
    }

instance Monoid Records where
    mempty = Records { fpCons = mempty
                  , tyCons = mempty
                  , typedProg = mempty
                  }

    mappend x y = Records { fpCons = fpCons x `mappend` fpCons y
                       , tyCons = tyCons x `mappend` tyCons y
                       , typedProg = typedProg x `mappend` typedProg y
                       }

type RecordsM = WriterT Records IO
type UsedM    = StateT Used RecordsM
type Result   = StateT Environment UsedM

-- Environment operations
addType :: VName -> Type -> Environment -> Environment
addType name ty env = Env $ (:) ((,) name ty) (types env)

lookupType :: VName -> Environment -> Type
lookupType name env = fromJust $ lookup name (types env)

-- Used operations
renewLabel :: Used -> (Label, Used)
renewLabel used = (oldLable, used { usedLabel = oldLable + 1 })
    where oldLable = usedLabel used

renewTypeName :: Used -> (TName, Used)
renewTypeName used = (oldTypeName, used { usedTypeNames = oldTypeName + 1 }) 
    where oldTypeName = usedTypeNames used

renewLabelName :: Used -> (LName, Used)
renewLabelName used = (oldLabelName, used { usedLabelNames = oldLabelName + 1 })
    where oldLabelName = usedLabelNames used

getLabel :: UsedM Label
getLabel = do
    used <- get
    let (label, renewUsed) = renewLabel used
    put renewUsed
    return label

getTypeName :: UsedM TName
getTypeName = do
    used <- get
    let (tname, renewUsed) = renewTypeName used
    put renewUsed
    return tname

getLabelName :: UsedM Fp
getLabelName = do
    used <- get
    let (lname, renewUsed) = renewLabelName used
    put renewUsed
    return $ Variable lname

getType :: UsedM Type
getType = do
    ty <- getTypeName
    fp <- getLabelName
    return $ TyVar ty fp

-- Records operations
createByFpCons :: FpCons -> Records
createByFpCons fpCons = Records
    { fpCons    = fpCons
    , tyCons    = mempty
    , typedProg = mempty
    }

createByTyCons :: TyCons -> Records
createByTyCons tyCons = Records
    { fpCons    = mempty
    , tyCons    = tyCons
    , typedProg = mempty
    }

createByTypedProg :: TypedProg -> Records
createByTypedProg tp = Records
    { fpCons    = mempty
    , tyCons    = mempty
    , typedProg = tp
    }

-- misc
makeFunTy :: [Type] -> Maybe Fp -> UsedM Type
makeFunTy = undefined
