{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--compile-spec" @-}

module Jacqueline.Health.Model
  ( migrateAll
  , mkUser
  , mkAddress
  , mkIndividual
  , mkBusinessAssociate
  , mkCoveredEntity
  , mkHospitalVisit
  , mkTreatment
  , mkDiagnosis
  , mkInformationTransferSet
  , mkTreatmentTransfer
  , mkDiagnosisTransfer
  , mkHospitalVisitTransfer
  , mkBusinessAssociateAgreement
  , mkTransaction
  , mkPersonalRepresentative
  , User
  , Address
  , Individual
  , BusinessAssociate
  , CoveredEntity
  , HospitalVisit
  , Treatment
  , Diagnosis
  , InformationTransferSet
  , TreatmentTransfer
  , DiagnosisTransfer
  , HospitalVisitTransfer
  , BusinessAssociateAgreement
  , Transaction
  , PersonalRepresentative
  , userId'
  , userUsername'
  , userEmail'
  , userProfileType'
  , userName'
  , userEntity'
  , userAssociate'
  , userIndividual'
  , addressId'
  , addressStreet'
  , addressCity'
  , addressState'
  , addressZipcode'
  , individualId'
  , individualFirstName'
  , individualLastName'
  , individualEmail'
  , individualAsddress'
  , individualBirthDate'
  , individualSsn'
  , individualThelephoneNumber'
  , individualFaxNumber'
  , individualDriversLicenseNumber'
  , individualEmployer'
  , individualReligiousAffiliation'
  , businessAssociateId'
  , businessAssociateName'
  , businessAssociateCovered'
  , coveredEntityId'
  , coveredEntityEin'
  , coveredEntityName'
  , hospitalVisitId'
  , hospitalVisitPatient'
  , hospitalVisitHospital'
  , hospitalVisitDateAdmitted'
  , hospitalVisitLocation'
  , hospitalVisitCondition'
  , hospitalVisitDateReleased'
  , treatmentId'
  , treatmentService'
  , treatmentDatePerformed'
  , treatmentPrescribingEntity'
  , treatmentPerformingEntity'
  , treatmentPatient'
  , diagnosisId'
  , diagnosisManifestation'
  , diagnosisDiagnosis'
  , diagnosisDateRecognized'
  , diagnosisRecognizingEntity'
  , diagnosisPatitent'
  , informationTransferSetId'
  , informationTransferSetData'
  , treatmentTransferId'
  , treatmentTransferSet'
  , treatmentTransferTreatment'
  , diagnosisTransferId'
  , diagnosisTransferSet'
  , diagnosisTransferDiagnosis'
  , hospitalVisitTransferId'
  , hospitalVisitTransferSet'
  , hospitalVisitTransferVisit'
  , businessAssociateAgreementId'
  , businessAssociateAgreementBusinessAssociate'
  , businessAssociateAgreementCoveredEntity'
  , businessAssociateAgreementSharedInformation'
  , businessAssociateAgreementPurpose'
  , transactionId'
  , transactionStandard'
  , transactionFirstParty'
  , transactionSecondParty'
  , transactionSharedInformation'
  , transactionDateRequested'
  , transactionDateResponded'
  , transactionPurpose'
  , personalRepresentativeId'
  , personalRepresentativeDependent'
  , personalRepresentativeRepresentative'
  , personalRepresentativeParent'
  , UserId
  , AddressId
  , IndividualId
  , BusinessAssociateId
  , CoveredEntityId
  , HospitalVisitId
  , TreatmentId
  , DiagnosisId
  , InformationTransferSetId
  , TreatmentTransferId
  , DiagnosisTransferId
  , HospitalVisitTransferId
  , BusinessAssociateAgreementId
  , TransactionId
  , PersonalRepresentativeId
  )
where


import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import qualified Database.Persist              as Persist

import           Storm.Core



--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  username String
  email String
  profileType Int
  name String
  entity CoveredEntityId Maybe
  associate BusinessAssociateId Maybe
  individual IndividualId Maybe
  

Address
  street String Maybe
  city String
  state String
  zipcode String
  

Individual
  firstName String
  lastName String
  email String Maybe
  asddress AddressId Maybe
  birthDate String Maybe
  ssn String Maybe
  thelephoneNumber String Maybe
  faxNumber String Maybe
  driversLicenseNumber String Maybe
  employer String Maybe
  religiousAffiliation String Maybe
  

BusinessAssociate
  name String
  covered Bool
  

CoveredEntity
  ein String
  name String
  

HospitalVisit
  patient IndividualId
  hospital CoveredEntityId
  dateAdmitted String Maybe
  location String Maybe
  condition String Maybe
  dateReleased String Maybe
  

Treatment
  service String
  datePerformed String
  prescribingEntity CoveredEntityId Maybe
  performingEntity CoveredEntityId Maybe
  patient IndividualId Maybe
  

Diagnosis
  manifestation String
  diagnosis String
  dateRecognized String
  recognizingEntity CoveredEntityId
  patitent IndividualId Maybe
  

InformationTransferSet
  data String
  

TreatmentTransfer
  set InformationTransferSetId
  treatment TreatmentId
  

DiagnosisTransfer
  set InformationTransferSetId
  diagnosis DiagnosisId
  

HospitalVisitTransfer
  set InformationTransferSetId
  visit HospitalVisitId
  

BusinessAssociateAgreement
  businessAssociate BusinessAssociateId
  coveredEntity CoveredEntityId
  sharedInformation InformationTransferSetId
  purpose String Maybe
  

Transaction
  standard String
  firstParty CoveredEntityId
  secondParty CoveredEntityId
  sharedInformation InformationTransferSetId
  dateRequested String
  dateResponded String
  purpose String
  

PersonalRepresentative
  dependent IndividualId
  representative IndividualId
  parent Bool
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: String
     -> x_1: String
     -> x_2: Int
     -> x_3: String
     -> x_4: Maybe CoveredEntityId
     -> x_5: Maybe BusinessAssociateId
     -> x_6: Maybe IndividualId
     -> StormRecord <{\row -> userUsername (entityVal row) == x_0 && userEmail (entityVal row) == x_1 && userProfileType (entityVal row) == x_2 && userName (entityVal row) == x_3 && userEntity (entityVal row) == x_4 && userAssociate (entityVal row) == x_5 && userIndividual (entityVal row) == x_6},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (x_0 == x_1)}>
                     (Entity User) User
  @-}
mkUser :: String -> String -> Int -> String -> Maybe CoveredEntityId -> Maybe BusinessAssociateId -> Maybe IndividualId -> StormRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 x_4 x_5 x_6 = StormRecord (User x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) User UserId
  @-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userUsername :: User -> String @-}

{-@ measure userUsernameCap :: Entity User -> Bool @-}

{-@ assume userUsername' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userUsername (entityVal row)},
                          {\field row -> field == userUsername (entityVal row)},
                          {\old -> userUsernameCap old},
                          {\old _ _ -> userUsernameCap old}>
                          (Entity User) User String
  @-}
userUsername' :: EntityFieldWrapper (Entity User) User String
userUsername' = EntityFieldWrapper UserUsername

{-@ measure userEmail :: User -> String @-}

{-@ measure userEmailCap :: Entity User -> Bool @-}

{-@ assume userEmail' ::
      EntityFieldWrapper <{\x_0 x_1 -> (x_0 == x_1)},
                          {\row field -> field == userEmail (entityVal row)},
                          {\field row -> field == userEmail (entityVal row)},
                          {\old -> userEmailCap old},
                          {\old _ _ -> userEmailCap old}>
                          (Entity User) User String
  @-}
userEmail' :: EntityFieldWrapper (Entity User) User String
userEmail' = EntityFieldWrapper UserEmail

{-@ measure userProfileType :: User -> Int @-}

{-@ measure userProfileTypeCap :: Entity User -> Bool @-}

{-@ assume userProfileType' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userProfileType (entityVal row)},
                          {\field row -> field == userProfileType (entityVal row)},
                          {\old -> userProfileTypeCap old},
                          {\old _ _ -> userProfileTypeCap old}>
                          (Entity User) User Int
  @-}
userProfileType' :: EntityFieldWrapper (Entity User) User Int
userProfileType' = EntityFieldWrapper UserProfileType

{-@ measure userName :: User -> String @-}

{-@ measure userNameCap :: Entity User -> Bool @-}

{-@ assume userName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userName (entityVal row)},
                          {\field row -> field == userName (entityVal row)},
                          {\old -> userNameCap old},
                          {\old _ _ -> userNameCap old}>
                          (Entity User) User String
  @-}
userName' :: EntityFieldWrapper (Entity User) User String
userName' = EntityFieldWrapper UserName

{-@ measure userEntity :: User -> (Maybe CoveredEntityId) @-}

{-@ measure userEntityCap :: Entity User -> Bool @-}

{-@ assume userEntity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userEntity (entityVal row)},
                          {\field row -> field == userEntity (entityVal row)},
                          {\old -> userEntityCap old},
                          {\old _ _ -> userEntityCap old}>
                          (Entity User) User (Maybe CoveredEntityId)
  @-}
userEntity' :: EntityFieldWrapper (Entity User) User (Maybe CoveredEntityId)
userEntity' = EntityFieldWrapper UserEntity

{-@ measure userAssociate :: User -> (Maybe BusinessAssociateId) @-}

{-@ measure userAssociateCap :: Entity User -> Bool @-}

{-@ assume userAssociate' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userAssociate (entityVal row)},
                          {\field row -> field == userAssociate (entityVal row)},
                          {\old -> userAssociateCap old},
                          {\old _ _ -> userAssociateCap old}>
                          (Entity User) User (Maybe BusinessAssociateId)
  @-}
userAssociate' :: EntityFieldWrapper (Entity User) User (Maybe BusinessAssociateId)
userAssociate' = EntityFieldWrapper UserAssociate

{-@ measure userIndividual :: User -> (Maybe IndividualId) @-}

{-@ measure userIndividualCap :: Entity User -> Bool @-}

{-@ assume userIndividual' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userIndividual (entityVal row)},
                          {\field row -> field == userIndividual (entityVal row)},
                          {\old -> userIndividualCap old},
                          {\old _ _ -> userIndividualCap old}>
                          (Entity User) User (Maybe IndividualId)
  @-}
userIndividual' :: EntityFieldWrapper (Entity User) User (Maybe IndividualId)
userIndividual' = EntityFieldWrapper UserIndividual

-- * Address
{-@ mkAddress ::
        x_0: Maybe String
     -> x_1: String
     -> x_2: String
     -> x_3: String
     -> StormRecord <{\row -> addressStreet (entityVal row) == x_0 && addressCity (entityVal row) == x_1 && addressState (entityVal row) == x_2 && addressZipcode (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Address
  @-}
mkAddress :: Maybe String -> String -> String -> String -> StormRecord (Entity User) Address
mkAddress x_0 x_1 x_2 x_3 = StormRecord (Address x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Address | v == getJust (entityKey v)} @-}



{-@ assume addressId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Address AddressId
  @-}
addressId' :: EntityFieldWrapper (Entity User) Address AddressId
addressId' = EntityFieldWrapper AddressId

{-@ measure addressStreet :: Address -> (Maybe String) @-}

{-@ measure addressStreetCap :: Entity Address -> Bool @-}

{-@ assume addressStreet' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == addressStreet (entityVal row)},
                          {\field row -> field == addressStreet (entityVal row)},
                          {\old -> addressStreetCap old},
                          {\old _ _ -> addressStreetCap old}>
                          (Entity User) Address (Maybe String)
  @-}
addressStreet' :: EntityFieldWrapper (Entity User) Address (Maybe String)
addressStreet' = EntityFieldWrapper AddressStreet

{-@ measure addressCity :: Address -> String @-}

{-@ measure addressCityCap :: Entity Address -> Bool @-}

{-@ assume addressCity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == addressCity (entityVal row)},
                          {\field row -> field == addressCity (entityVal row)},
                          {\old -> addressCityCap old},
                          {\old _ _ -> addressCityCap old}>
                          (Entity User) Address String
  @-}
addressCity' :: EntityFieldWrapper (Entity User) Address String
addressCity' = EntityFieldWrapper AddressCity

{-@ measure addressState :: Address -> String @-}

{-@ measure addressStateCap :: Entity Address -> Bool @-}

{-@ assume addressState' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == addressState (entityVal row)},
                          {\field row -> field == addressState (entityVal row)},
                          {\old -> addressStateCap old},
                          {\old _ _ -> addressStateCap old}>
                          (Entity User) Address String
  @-}
addressState' :: EntityFieldWrapper (Entity User) Address String
addressState' = EntityFieldWrapper AddressState

{-@ measure addressZipcode :: Address -> String @-}

{-@ measure addressZipcodeCap :: Entity Address -> Bool @-}

{-@ assume addressZipcode' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == addressZipcode (entityVal row)},
                          {\field row -> field == addressZipcode (entityVal row)},
                          {\old -> addressZipcodeCap old},
                          {\old _ _ -> addressZipcodeCap old}>
                          (Entity User) Address String
  @-}
addressZipcode' :: EntityFieldWrapper (Entity User) Address String
addressZipcode' = EntityFieldWrapper AddressZipcode

-- * Individual
{-@ mkIndividual ::
        x_0: String
     -> x_1: String
     -> x_2: Maybe String
     -> x_3: Maybe AddressId
     -> x_4: Maybe String
     -> x_5: Maybe String
     -> x_6: Maybe String
     -> x_7: Maybe String
     -> x_8: Maybe String
     -> x_9: Maybe String
     -> x_10: Maybe String
     -> StormRecord <{\row -> individualFirstName (entityVal row) == x_0 && individualLastName (entityVal row) == x_1 && individualEmail (entityVal row) == x_2 && individualAsddress (entityVal row) == x_3 && individualBirthDate (entityVal row) == x_4 && individualSsn (entityVal row) == x_5 && individualThelephoneNumber (entityVal row) == x_6 && individualFaxNumber (entityVal row) == x_7 && individualDriversLicenseNumber (entityVal row) == x_8 && individualEmployer (entityVal row) == x_9 && individualReligiousAffiliation (entityVal row) == x_10},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)}>
                     (Entity User) Individual
  @-}
mkIndividual :: String -> String -> Maybe String -> Maybe AddressId -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> StormRecord (Entity User) Individual
mkIndividual x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 = StormRecord (Individual x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10)

{-@ invariant {v: Entity Individual | v == getJust (entityKey v)} @-}



{-@ assume individualId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Individual IndividualId
  @-}
individualId' :: EntityFieldWrapper (Entity User) Individual IndividualId
individualId' = EntityFieldWrapper IndividualId

{-@ measure individualFirstName :: Individual -> String @-}

{-@ measure individualFirstNameCap :: Entity Individual -> Bool @-}

{-@ assume individualFirstName' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualFirstName (entityVal row)},
                          {\field row -> field == individualFirstName (entityVal row)},
                          {\old -> individualFirstNameCap old},
                          {\old _ _ -> individualFirstNameCap old}>
                          (Entity User) Individual String
  @-}
individualFirstName' :: EntityFieldWrapper (Entity User) Individual String
individualFirstName' = EntityFieldWrapper IndividualFirstName

{-@ measure individualLastName :: Individual -> String @-}

{-@ measure individualLastNameCap :: Entity Individual -> Bool @-}

{-@ assume individualLastName' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualLastName (entityVal row)},
                          {\field row -> field == individualLastName (entityVal row)},
                          {\old -> individualLastNameCap old},
                          {\old _ _ -> individualLastNameCap old}>
                          (Entity User) Individual String
  @-}
individualLastName' :: EntityFieldWrapper (Entity User) Individual String
individualLastName' = EntityFieldWrapper IndividualLastName

{-@ measure individualEmail :: Individual -> (Maybe String) @-}

{-@ measure individualEmailCap :: Entity Individual -> Bool @-}

{-@ assume individualEmail' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualEmail (entityVal row)},
                          {\field row -> field == individualEmail (entityVal row)},
                          {\old -> individualEmailCap old},
                          {\old _ _ -> individualEmailCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualEmail' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualEmail' = EntityFieldWrapper IndividualEmail

{-@ measure individualAsddress :: Individual -> (Maybe AddressId) @-}

{-@ measure individualAsddressCap :: Entity Individual -> Bool @-}

{-@ assume individualAsddress' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == individualAsddress (entityVal row)},
                          {\field row -> field == individualAsddress (entityVal row)},
                          {\old -> individualAsddressCap old},
                          {\old _ _ -> individualAsddressCap old}>
                          (Entity User) Individual (Maybe AddressId)
  @-}
individualAsddress' :: EntityFieldWrapper (Entity User) Individual (Maybe AddressId)
individualAsddress' = EntityFieldWrapper IndividualAsddress

{-@ measure individualBirthDate :: Individual -> (Maybe String) @-}

{-@ measure individualBirthDateCap :: Entity Individual -> Bool @-}

{-@ assume individualBirthDate' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == individualBirthDate (entityVal row)},
                          {\field row -> field == individualBirthDate (entityVal row)},
                          {\old -> individualBirthDateCap old},
                          {\old _ _ -> individualBirthDateCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualBirthDate' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualBirthDate' = EntityFieldWrapper IndividualBirthDate

{-@ measure individualSsn :: Individual -> (Maybe String) @-}

{-@ measure individualSsnCap :: Entity Individual -> Bool @-}

{-@ assume individualSsn' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualSsn (entityVal row)},
                          {\field row -> field == individualSsn (entityVal row)},
                          {\old -> individualSsnCap old},
                          {\old _ _ -> individualSsnCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualSsn' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualSsn' = EntityFieldWrapper IndividualSsn

{-@ measure individualThelephoneNumber :: Individual -> (Maybe String) @-}

{-@ measure individualThelephoneNumberCap :: Entity Individual -> Bool @-}

{-@ assume individualThelephoneNumber' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == individualThelephoneNumber (entityVal row)},
                          {\field row -> field == individualThelephoneNumber (entityVal row)},
                          {\old -> individualThelephoneNumberCap old},
                          {\old _ _ -> individualThelephoneNumberCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualThelephoneNumber' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualThelephoneNumber' = EntityFieldWrapper IndividualThelephoneNumber

{-@ measure individualFaxNumber :: Individual -> (Maybe String) @-}

{-@ measure individualFaxNumberCap :: Entity Individual -> Bool @-}

{-@ assume individualFaxNumber' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualFaxNumber (entityVal row)},
                          {\field row -> field == individualFaxNumber (entityVal row)},
                          {\old -> individualFaxNumberCap old},
                          {\old _ _ -> individualFaxNumberCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualFaxNumber' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualFaxNumber' = EntityFieldWrapper IndividualFaxNumber

{-@ measure individualDriversLicenseNumber :: Individual -> (Maybe String) @-}

{-@ measure individualDriversLicenseNumberCap :: Entity Individual -> Bool @-}

{-@ assume individualDriversLicenseNumber' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualDriversLicenseNumber (entityVal row)},
                          {\field row -> field == individualDriversLicenseNumber (entityVal row)},
                          {\old -> individualDriversLicenseNumberCap old},
                          {\old _ _ -> individualDriversLicenseNumberCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualDriversLicenseNumber' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualDriversLicenseNumber' = EntityFieldWrapper IndividualDriversLicenseNumber

{-@ measure individualEmployer :: Individual -> (Maybe String) @-}

{-@ measure individualEmployerCap :: Entity Individual -> Bool @-}

{-@ assume individualEmployer' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualEmployer (entityVal row)},
                          {\field row -> field == individualEmployer (entityVal row)},
                          {\old -> individualEmployerCap old},
                          {\old _ _ -> individualEmployerCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualEmployer' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualEmployer' = EntityFieldWrapper IndividualEmployer

{-@ measure individualReligiousAffiliation :: Individual -> (Maybe String) @-}

{-@ measure individualReligiousAffiliationCap :: Entity Individual -> Bool @-}

{-@ assume individualReligiousAffiliation' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 1 || fromJust (userIndividual (entityVal x_1)) == entityKey x_0)},
                          {\row field -> field == individualReligiousAffiliation (entityVal row)},
                          {\field row -> field == individualReligiousAffiliation (entityVal row)},
                          {\old -> individualReligiousAffiliationCap old},
                          {\old _ _ -> individualReligiousAffiliationCap old}>
                          (Entity User) Individual (Maybe String)
  @-}
individualReligiousAffiliation' :: EntityFieldWrapper (Entity User) Individual (Maybe String)
individualReligiousAffiliation' = EntityFieldWrapper IndividualReligiousAffiliation

-- * BusinessAssociate
{-@ mkBusinessAssociate ::
        x_0: String
     -> x_1: Bool
     -> StormRecord <{\row -> businessAssociateName (entityVal row) == x_0 && businessAssociateCovered (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) BusinessAssociate
  @-}
mkBusinessAssociate :: String -> Bool -> StormRecord (Entity User) BusinessAssociate
mkBusinessAssociate x_0 x_1 = StormRecord (BusinessAssociate x_0 x_1)

{-@ invariant {v: Entity BusinessAssociate | v == getJust (entityKey v)} @-}



{-@ assume businessAssociateId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BusinessAssociate BusinessAssociateId
  @-}
businessAssociateId' :: EntityFieldWrapper (Entity User) BusinessAssociate BusinessAssociateId
businessAssociateId' = EntityFieldWrapper BusinessAssociateId

{-@ measure businessAssociateName :: BusinessAssociate -> String @-}

{-@ measure businessAssociateNameCap :: Entity BusinessAssociate -> Bool @-}

{-@ assume businessAssociateName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == businessAssociateName (entityVal row)},
                          {\field row -> field == businessAssociateName (entityVal row)},
                          {\old -> businessAssociateNameCap old},
                          {\old _ _ -> businessAssociateNameCap old}>
                          (Entity User) BusinessAssociate String
  @-}
businessAssociateName' :: EntityFieldWrapper (Entity User) BusinessAssociate String
businessAssociateName' = EntityFieldWrapper BusinessAssociateName

{-@ measure businessAssociateCovered :: BusinessAssociate -> Bool @-}

{-@ measure businessAssociateCoveredCap :: Entity BusinessAssociate -> Bool @-}

{-@ assume businessAssociateCovered' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == businessAssociateCovered (entityVal row)},
                          {\field row -> field == businessAssociateCovered (entityVal row)},
                          {\old -> businessAssociateCoveredCap old},
                          {\old _ _ -> businessAssociateCoveredCap old}>
                          (Entity User) BusinessAssociate Bool
  @-}
businessAssociateCovered' :: EntityFieldWrapper (Entity User) BusinessAssociate Bool
businessAssociateCovered' = EntityFieldWrapper BusinessAssociateCovered

-- * CoveredEntity
{-@ mkCoveredEntity ::
        x_0: String
     -> x_1: String
     -> StormRecord <{\row -> coveredEntityEin (entityVal row) == x_0 && coveredEntityName (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) CoveredEntity
  @-}
mkCoveredEntity :: String -> String -> StormRecord (Entity User) CoveredEntity
mkCoveredEntity x_0 x_1 = StormRecord (CoveredEntity x_0 x_1)

{-@ invariant {v: Entity CoveredEntity | v == getJust (entityKey v)} @-}



{-@ assume coveredEntityId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) CoveredEntity CoveredEntityId
  @-}
coveredEntityId' :: EntityFieldWrapper (Entity User) CoveredEntity CoveredEntityId
coveredEntityId' = EntityFieldWrapper CoveredEntityId

{-@ measure coveredEntityEin :: CoveredEntity -> String @-}

{-@ measure coveredEntityEinCap :: Entity CoveredEntity -> Bool @-}

{-@ assume coveredEntityEin' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == coveredEntityEin (entityVal row)},
                          {\field row -> field == coveredEntityEin (entityVal row)},
                          {\old -> coveredEntityEinCap old},
                          {\old _ _ -> coveredEntityEinCap old}>
                          (Entity User) CoveredEntity String
  @-}
coveredEntityEin' :: EntityFieldWrapper (Entity User) CoveredEntity String
coveredEntityEin' = EntityFieldWrapper CoveredEntityEin

{-@ measure coveredEntityName :: CoveredEntity -> String @-}

{-@ measure coveredEntityNameCap :: Entity CoveredEntity -> Bool @-}

{-@ assume coveredEntityName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == coveredEntityName (entityVal row)},
                          {\field row -> field == coveredEntityName (entityVal row)},
                          {\old -> coveredEntityNameCap old},
                          {\old _ _ -> coveredEntityNameCap old}>
                          (Entity User) CoveredEntity String
  @-}
coveredEntityName' :: EntityFieldWrapper (Entity User) CoveredEntity String
coveredEntityName' = EntityFieldWrapper CoveredEntityName

-- * HospitalVisit
{-@ mkHospitalVisit ::
        x_0: IndividualId
     -> x_1: CoveredEntityId
     -> x_2: Maybe String
     -> x_3: Maybe String
     -> x_4: Maybe String
     -> x_5: Maybe String
     -> StormRecord <{\row -> hospitalVisitPatient (entityVal row) == x_0 && hospitalVisitHospital (entityVal row) == x_1 && hospitalVisitDateAdmitted (entityVal row) == x_2 && hospitalVisitLocation (entityVal row) == x_3 && hospitalVisitCondition (entityVal row) == x_4 && hospitalVisitDateReleased (entityVal row) == x_5},
                     {\_ _ -> True},
                     {\x_0 x_1 -> ((userProfileType (entityVal x_1) == 1 => hospitalVisitPatient (entityVal x_0) == fromJust (userIndividual (entityVal x_1))) || (userProfileType (entityVal x_1) == 2 => hospitalVisitHospital (entityVal x_0) == fromJust (userEntity (entityVal x_1))) || userProfileType (entityVal x_1) == 6)}>
                     (Entity User) HospitalVisit
  @-}
mkHospitalVisit :: IndividualId -> CoveredEntityId -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> StormRecord (Entity User) HospitalVisit
mkHospitalVisit x_0 x_1 x_2 x_3 x_4 x_5 = StormRecord (HospitalVisit x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity HospitalVisit | v == getJust (entityKey v)} @-}



{-@ assume hospitalVisitId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) HospitalVisit HospitalVisitId
  @-}
hospitalVisitId' :: EntityFieldWrapper (Entity User) HospitalVisit HospitalVisitId
hospitalVisitId' = EntityFieldWrapper HospitalVisitId

{-@ measure hospitalVisitPatient :: HospitalVisit -> IndividualId @-}

{-@ measure hospitalVisitPatientCap :: Entity HospitalVisit -> Bool @-}

{-@ assume hospitalVisitPatient' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitPatient (entityVal row)},
                          {\field row -> field == hospitalVisitPatient (entityVal row)},
                          {\old -> hospitalVisitPatientCap old},
                          {\old _ _ -> hospitalVisitPatientCap old}>
                          (Entity User) HospitalVisit IndividualId
  @-}
hospitalVisitPatient' :: EntityFieldWrapper (Entity User) HospitalVisit IndividualId
hospitalVisitPatient' = EntityFieldWrapper HospitalVisitPatient

{-@ measure hospitalVisitHospital :: HospitalVisit -> CoveredEntityId @-}

{-@ measure hospitalVisitHospitalCap :: Entity HospitalVisit -> Bool @-}

{-@ assume hospitalVisitHospital' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitHospital (entityVal row)},
                          {\field row -> field == hospitalVisitHospital (entityVal row)},
                          {\old -> hospitalVisitHospitalCap old},
                          {\old _ _ -> hospitalVisitHospitalCap old}>
                          (Entity User) HospitalVisit CoveredEntityId
  @-}
hospitalVisitHospital' :: EntityFieldWrapper (Entity User) HospitalVisit CoveredEntityId
hospitalVisitHospital' = EntityFieldWrapper HospitalVisitHospital

{-@ measure hospitalVisitDateAdmitted :: HospitalVisit -> (Maybe String) @-}

{-@ measure hospitalVisitDateAdmittedCap :: Entity HospitalVisit -> Bool @-}

{-@ assume hospitalVisitDateAdmitted' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitDateAdmitted (entityVal row)},
                          {\field row -> field == hospitalVisitDateAdmitted (entityVal row)},
                          {\old -> hospitalVisitDateAdmittedCap old},
                          {\old _ _ -> hospitalVisitDateAdmittedCap old}>
                          (Entity User) HospitalVisit (Maybe String)
  @-}
hospitalVisitDateAdmitted' :: EntityFieldWrapper (Entity User) HospitalVisit (Maybe String)
hospitalVisitDateAdmitted' = EntityFieldWrapper HospitalVisitDateAdmitted

{-@ measure hospitalVisitLocation :: HospitalVisit -> (Maybe String) @-}

{-@ measure hospitalVisitLocationCap :: Entity HospitalVisit -> Bool @-}

{-@ assume hospitalVisitLocation' ::
      EntityFieldWrapper <{\x_0 x_1 -> ((userProfileType (entityVal x_1) == 1 => hospitalVisitPatient (entityVal x_0) == fromJust (userIndividual (entityVal x_1))) || (userProfileType (entityVal x_1) == 2 => hospitalVisitHospital (entityVal x_0) == fromJust (userEntity (entityVal x_1))) || userProfileType (entityVal x_1) == 6)},
                          {\row field -> field == hospitalVisitLocation (entityVal row)},
                          {\field row -> field == hospitalVisitLocation (entityVal row)},
                          {\old -> hospitalVisitLocationCap old},
                          {\old _ _ -> hospitalVisitLocationCap old}>
                          (Entity User) HospitalVisit (Maybe String)
  @-}
hospitalVisitLocation' :: EntityFieldWrapper (Entity User) HospitalVisit (Maybe String)
hospitalVisitLocation' = EntityFieldWrapper HospitalVisitLocation

{-@ measure hospitalVisitCondition :: HospitalVisit -> (Maybe String) @-}

{-@ measure hospitalVisitConditionCap :: Entity HospitalVisit -> Bool @-}

{-@ assume hospitalVisitCondition' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitCondition (entityVal row)},
                          {\field row -> field == hospitalVisitCondition (entityVal row)},
                          {\old -> hospitalVisitConditionCap old},
                          {\old _ _ -> hospitalVisitConditionCap old}>
                          (Entity User) HospitalVisit (Maybe String)
  @-}
hospitalVisitCondition' :: EntityFieldWrapper (Entity User) HospitalVisit (Maybe String)
hospitalVisitCondition' = EntityFieldWrapper HospitalVisitCondition

{-@ measure hospitalVisitDateReleased :: HospitalVisit -> (Maybe String) @-}

{-@ measure hospitalVisitDateReleasedCap :: Entity HospitalVisit -> Bool @-}

{-@ assume hospitalVisitDateReleased' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitDateReleased (entityVal row)},
                          {\field row -> field == hospitalVisitDateReleased (entityVal row)},
                          {\old -> hospitalVisitDateReleasedCap old},
                          {\old _ _ -> hospitalVisitDateReleasedCap old}>
                          (Entity User) HospitalVisit (Maybe String)
  @-}
hospitalVisitDateReleased' :: EntityFieldWrapper (Entity User) HospitalVisit (Maybe String)
hospitalVisitDateReleased' = EntityFieldWrapper HospitalVisitDateReleased

-- * Treatment
{-@ mkTreatment ::
        x_0: String
     -> x_1: String
     -> x_2: Maybe CoveredEntityId
     -> x_3: Maybe CoveredEntityId
     -> x_4: Maybe IndividualId
     -> StormRecord <{\row -> treatmentService (entityVal row) == x_0 && treatmentDatePerformed (entityVal row) == x_1 && treatmentPrescribingEntity (entityVal row) == x_2 && treatmentPerformingEntity (entityVal row) == x_3 && treatmentPatient (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> ((userProfileType (entityVal x_1) == 1 => treatmentPatient (entityVal x_0) == userIndividual (entityVal x_1)) || (userProfileType (entityVal x_1) == 2 => treatmentPrescribingEntity (entityVal x_0) == userEntity (entityVal x_1)))}>
                     (Entity User) Treatment
  @-}
mkTreatment :: String -> String -> Maybe CoveredEntityId -> Maybe CoveredEntityId -> Maybe IndividualId -> StormRecord (Entity User) Treatment
mkTreatment x_0 x_1 x_2 x_3 x_4 = StormRecord (Treatment x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity Treatment | v == getJust (entityKey v)} @-}



{-@ assume treatmentId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Treatment TreatmentId
  @-}
treatmentId' :: EntityFieldWrapper (Entity User) Treatment TreatmentId
treatmentId' = EntityFieldWrapper TreatmentId

{-@ measure treatmentService :: Treatment -> String @-}

{-@ measure treatmentServiceCap :: Entity Treatment -> Bool @-}

{-@ assume treatmentService' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == treatmentService (entityVal row)},
                          {\field row -> field == treatmentService (entityVal row)},
                          {\old -> treatmentServiceCap old},
                          {\old _ _ -> treatmentServiceCap old}>
                          (Entity User) Treatment String
  @-}
treatmentService' :: EntityFieldWrapper (Entity User) Treatment String
treatmentService' = EntityFieldWrapper TreatmentService

{-@ measure treatmentDatePerformed :: Treatment -> String @-}

{-@ measure treatmentDatePerformedCap :: Entity Treatment -> Bool @-}

{-@ assume treatmentDatePerformed' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == treatmentDatePerformed (entityVal row)},
                          {\field row -> field == treatmentDatePerformed (entityVal row)},
                          {\old -> treatmentDatePerformedCap old},
                          {\old _ _ -> treatmentDatePerformedCap old}>
                          (Entity User) Treatment String
  @-}
treatmentDatePerformed' :: EntityFieldWrapper (Entity User) Treatment String
treatmentDatePerformed' = EntityFieldWrapper TreatmentDatePerformed

{-@ measure treatmentPrescribingEntity :: Treatment -> (Maybe CoveredEntityId) @-}

{-@ measure treatmentPrescribingEntityCap :: Entity Treatment -> Bool @-}

{-@ assume treatmentPrescribingEntity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == treatmentPrescribingEntity (entityVal row)},
                          {\field row -> field == treatmentPrescribingEntity (entityVal row)},
                          {\old -> treatmentPrescribingEntityCap old},
                          {\old _ _ -> treatmentPrescribingEntityCap old}>
                          (Entity User) Treatment (Maybe CoveredEntityId)
  @-}
treatmentPrescribingEntity' :: EntityFieldWrapper (Entity User) Treatment (Maybe CoveredEntityId)
treatmentPrescribingEntity' = EntityFieldWrapper TreatmentPrescribingEntity

{-@ measure treatmentPerformingEntity :: Treatment -> (Maybe CoveredEntityId) @-}

{-@ measure treatmentPerformingEntityCap :: Entity Treatment -> Bool @-}

{-@ assume treatmentPerformingEntity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == treatmentPerformingEntity (entityVal row)},
                          {\field row -> field == treatmentPerformingEntity (entityVal row)},
                          {\old -> treatmentPerformingEntityCap old},
                          {\old _ _ -> treatmentPerformingEntityCap old}>
                          (Entity User) Treatment (Maybe CoveredEntityId)
  @-}
treatmentPerformingEntity' :: EntityFieldWrapper (Entity User) Treatment (Maybe CoveredEntityId)
treatmentPerformingEntity' = EntityFieldWrapper TreatmentPerformingEntity

{-@ measure treatmentPatient :: Treatment -> (Maybe IndividualId) @-}

{-@ measure treatmentPatientCap :: Entity Treatment -> Bool @-}

{-@ assume treatmentPatient' ::
      EntityFieldWrapper <{\x_0 x_1 -> ((userProfileType (entityVal x_1) == 1 => treatmentPatient (entityVal x_0) == userIndividual (entityVal x_1)) || (userProfileType (entityVal x_1) == 2 => treatmentPrescribingEntity (entityVal x_0) == userEntity (entityVal x_1)))},
                          {\row field -> field == treatmentPatient (entityVal row)},
                          {\field row -> field == treatmentPatient (entityVal row)},
                          {\old -> treatmentPatientCap old},
                          {\old _ _ -> treatmentPatientCap old}>
                          (Entity User) Treatment (Maybe IndividualId)
  @-}
treatmentPatient' :: EntityFieldWrapper (Entity User) Treatment (Maybe IndividualId)
treatmentPatient' = EntityFieldWrapper TreatmentPatient

-- * Diagnosis
{-@ mkDiagnosis ::
        x_0: String
     -> x_1: String
     -> x_2: String
     -> x_3: CoveredEntityId
     -> x_4: Maybe IndividualId
     -> StormRecord <{\row -> diagnosisManifestation (entityVal row) == x_0 && diagnosisDiagnosis (entityVal row) == x_1 && diagnosisDateRecognized (entityVal row) == x_2 && diagnosisRecognizingEntity (entityVal row) == x_3 && diagnosisPatitent (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Diagnosis
  @-}
mkDiagnosis :: String -> String -> String -> CoveredEntityId -> Maybe IndividualId -> StormRecord (Entity User) Diagnosis
mkDiagnosis x_0 x_1 x_2 x_3 x_4 = StormRecord (Diagnosis x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity Diagnosis | v == getJust (entityKey v)} @-}



{-@ assume diagnosisId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Diagnosis DiagnosisId
  @-}
diagnosisId' :: EntityFieldWrapper (Entity User) Diagnosis DiagnosisId
diagnosisId' = EntityFieldWrapper DiagnosisId

{-@ measure diagnosisManifestation :: Diagnosis -> String @-}

{-@ measure diagnosisManifestationCap :: Entity Diagnosis -> Bool @-}

{-@ assume diagnosisManifestation' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisManifestation (entityVal row)},
                          {\field row -> field == diagnosisManifestation (entityVal row)},
                          {\old -> diagnosisManifestationCap old},
                          {\old _ _ -> diagnosisManifestationCap old}>
                          (Entity User) Diagnosis String
  @-}
diagnosisManifestation' :: EntityFieldWrapper (Entity User) Diagnosis String
diagnosisManifestation' = EntityFieldWrapper DiagnosisManifestation

{-@ measure diagnosisDiagnosis :: Diagnosis -> String @-}

{-@ measure diagnosisDiagnosisCap :: Entity Diagnosis -> Bool @-}

{-@ assume diagnosisDiagnosis' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisDiagnosis (entityVal row)},
                          {\field row -> field == diagnosisDiagnosis (entityVal row)},
                          {\old -> diagnosisDiagnosisCap old},
                          {\old _ _ -> diagnosisDiagnosisCap old}>
                          (Entity User) Diagnosis String
  @-}
diagnosisDiagnosis' :: EntityFieldWrapper (Entity User) Diagnosis String
diagnosisDiagnosis' = EntityFieldWrapper DiagnosisDiagnosis

{-@ measure diagnosisDateRecognized :: Diagnosis -> String @-}

{-@ measure diagnosisDateRecognizedCap :: Entity Diagnosis -> Bool @-}

{-@ assume diagnosisDateRecognized' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisDateRecognized (entityVal row)},
                          {\field row -> field == diagnosisDateRecognized (entityVal row)},
                          {\old -> diagnosisDateRecognizedCap old},
                          {\old _ _ -> diagnosisDateRecognizedCap old}>
                          (Entity User) Diagnosis String
  @-}
diagnosisDateRecognized' :: EntityFieldWrapper (Entity User) Diagnosis String
diagnosisDateRecognized' = EntityFieldWrapper DiagnosisDateRecognized

{-@ measure diagnosisRecognizingEntity :: Diagnosis -> CoveredEntityId @-}

{-@ measure diagnosisRecognizingEntityCap :: Entity Diagnosis -> Bool @-}

{-@ assume diagnosisRecognizingEntity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisRecognizingEntity (entityVal row)},
                          {\field row -> field == diagnosisRecognizingEntity (entityVal row)},
                          {\old -> diagnosisRecognizingEntityCap old},
                          {\old _ _ -> diagnosisRecognizingEntityCap old}>
                          (Entity User) Diagnosis CoveredEntityId
  @-}
diagnosisRecognizingEntity' :: EntityFieldWrapper (Entity User) Diagnosis CoveredEntityId
diagnosisRecognizingEntity' = EntityFieldWrapper DiagnosisRecognizingEntity

{-@ measure diagnosisPatitent :: Diagnosis -> (Maybe IndividualId) @-}

{-@ measure diagnosisPatitentCap :: Entity Diagnosis -> Bool @-}

{-@ assume diagnosisPatitent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisPatitent (entityVal row)},
                          {\field row -> field == diagnosisPatitent (entityVal row)},
                          {\old -> diagnosisPatitentCap old},
                          {\old _ _ -> diagnosisPatitentCap old}>
                          (Entity User) Diagnosis (Maybe IndividualId)
  @-}
diagnosisPatitent' :: EntityFieldWrapper (Entity User) Diagnosis (Maybe IndividualId)
diagnosisPatitent' = EntityFieldWrapper DiagnosisPatitent

-- * InformationTransferSet
{-@ mkInformationTransferSet ::
        x_0: String
     -> StormRecord <{\row -> informationTransferSetData (entityVal row) == x_0},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) InformationTransferSet
  @-}
mkInformationTransferSet :: String -> StormRecord (Entity User) InformationTransferSet
mkInformationTransferSet x_0 = StormRecord (InformationTransferSet x_0)

{-@ invariant {v: Entity InformationTransferSet | v == getJust (entityKey v)} @-}



{-@ assume informationTransferSetId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) InformationTransferSet InformationTransferSetId
  @-}
informationTransferSetId' :: EntityFieldWrapper (Entity User) InformationTransferSet InformationTransferSetId
informationTransferSetId' = EntityFieldWrapper InformationTransferSetId

{-@ measure informationTransferSetData :: InformationTransferSet -> String @-}

{-@ measure informationTransferSetDataCap :: Entity InformationTransferSet -> Bool @-}

{-@ assume informationTransferSetData' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == informationTransferSetData (entityVal row)},
                          {\field row -> field == informationTransferSetData (entityVal row)},
                          {\old -> informationTransferSetDataCap old},
                          {\old _ _ -> informationTransferSetDataCap old}>
                          (Entity User) InformationTransferSet String
  @-}
informationTransferSetData' :: EntityFieldWrapper (Entity User) InformationTransferSet String
informationTransferSetData' = EntityFieldWrapper InformationTransferSetData

-- * TreatmentTransfer
{-@ mkTreatmentTransfer ::
        x_0: InformationTransferSetId
     -> x_1: TreatmentId
     -> StormRecord <{\row -> treatmentTransferSet (entityVal row) == x_0 && treatmentTransferTreatment (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) TreatmentTransfer
  @-}
mkTreatmentTransfer :: InformationTransferSetId -> TreatmentId -> StormRecord (Entity User) TreatmentTransfer
mkTreatmentTransfer x_0 x_1 = StormRecord (TreatmentTransfer x_0 x_1)

{-@ invariant {v: Entity TreatmentTransfer | v == getJust (entityKey v)} @-}



{-@ assume treatmentTransferId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TreatmentTransfer TreatmentTransferId
  @-}
treatmentTransferId' :: EntityFieldWrapper (Entity User) TreatmentTransfer TreatmentTransferId
treatmentTransferId' = EntityFieldWrapper TreatmentTransferId

{-@ measure treatmentTransferSet :: TreatmentTransfer -> InformationTransferSetId @-}

{-@ measure treatmentTransferSetCap :: Entity TreatmentTransfer -> Bool @-}

{-@ assume treatmentTransferSet' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == treatmentTransferSet (entityVal row)},
                          {\field row -> field == treatmentTransferSet (entityVal row)},
                          {\old -> treatmentTransferSetCap old},
                          {\old _ _ -> treatmentTransferSetCap old}>
                          (Entity User) TreatmentTransfer InformationTransferSetId
  @-}
treatmentTransferSet' :: EntityFieldWrapper (Entity User) TreatmentTransfer InformationTransferSetId
treatmentTransferSet' = EntityFieldWrapper TreatmentTransferSet

{-@ measure treatmentTransferTreatment :: TreatmentTransfer -> TreatmentId @-}

{-@ measure treatmentTransferTreatmentCap :: Entity TreatmentTransfer -> Bool @-}

{-@ assume treatmentTransferTreatment' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == treatmentTransferTreatment (entityVal row)},
                          {\field row -> field == treatmentTransferTreatment (entityVal row)},
                          {\old -> treatmentTransferTreatmentCap old},
                          {\old _ _ -> treatmentTransferTreatmentCap old}>
                          (Entity User) TreatmentTransfer TreatmentId
  @-}
treatmentTransferTreatment' :: EntityFieldWrapper (Entity User) TreatmentTransfer TreatmentId
treatmentTransferTreatment' = EntityFieldWrapper TreatmentTransferTreatment

-- * DiagnosisTransfer
{-@ mkDiagnosisTransfer ::
        x_0: InformationTransferSetId
     -> x_1: DiagnosisId
     -> StormRecord <{\row -> diagnosisTransferSet (entityVal row) == x_0 && diagnosisTransferDiagnosis (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) DiagnosisTransfer
  @-}
mkDiagnosisTransfer :: InformationTransferSetId -> DiagnosisId -> StormRecord (Entity User) DiagnosisTransfer
mkDiagnosisTransfer x_0 x_1 = StormRecord (DiagnosisTransfer x_0 x_1)

{-@ invariant {v: Entity DiagnosisTransfer | v == getJust (entityKey v)} @-}



{-@ assume diagnosisTransferId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) DiagnosisTransfer DiagnosisTransferId
  @-}
diagnosisTransferId' :: EntityFieldWrapper (Entity User) DiagnosisTransfer DiagnosisTransferId
diagnosisTransferId' = EntityFieldWrapper DiagnosisTransferId

{-@ measure diagnosisTransferSet :: DiagnosisTransfer -> InformationTransferSetId @-}

{-@ measure diagnosisTransferSetCap :: Entity DiagnosisTransfer -> Bool @-}

{-@ assume diagnosisTransferSet' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisTransferSet (entityVal row)},
                          {\field row -> field == diagnosisTransferSet (entityVal row)},
                          {\old -> diagnosisTransferSetCap old},
                          {\old _ _ -> diagnosisTransferSetCap old}>
                          (Entity User) DiagnosisTransfer InformationTransferSetId
  @-}
diagnosisTransferSet' :: EntityFieldWrapper (Entity User) DiagnosisTransfer InformationTransferSetId
diagnosisTransferSet' = EntityFieldWrapper DiagnosisTransferSet

{-@ measure diagnosisTransferDiagnosis :: DiagnosisTransfer -> DiagnosisId @-}

{-@ measure diagnosisTransferDiagnosisCap :: Entity DiagnosisTransfer -> Bool @-}

{-@ assume diagnosisTransferDiagnosis' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == diagnosisTransferDiagnosis (entityVal row)},
                          {\field row -> field == diagnosisTransferDiagnosis (entityVal row)},
                          {\old -> diagnosisTransferDiagnosisCap old},
                          {\old _ _ -> diagnosisTransferDiagnosisCap old}>
                          (Entity User) DiagnosisTransfer DiagnosisId
  @-}
diagnosisTransferDiagnosis' :: EntityFieldWrapper (Entity User) DiagnosisTransfer DiagnosisId
diagnosisTransferDiagnosis' = EntityFieldWrapper DiagnosisTransferDiagnosis

-- * HospitalVisitTransfer
{-@ mkHospitalVisitTransfer ::
        x_0: InformationTransferSetId
     -> x_1: HospitalVisitId
     -> StormRecord <{\row -> hospitalVisitTransferSet (entityVal row) == x_0 && hospitalVisitTransferVisit (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) HospitalVisitTransfer
  @-}
mkHospitalVisitTransfer :: InformationTransferSetId -> HospitalVisitId -> StormRecord (Entity User) HospitalVisitTransfer
mkHospitalVisitTransfer x_0 x_1 = StormRecord (HospitalVisitTransfer x_0 x_1)

{-@ invariant {v: Entity HospitalVisitTransfer | v == getJust (entityKey v)} @-}



{-@ assume hospitalVisitTransferId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) HospitalVisitTransfer HospitalVisitTransferId
  @-}
hospitalVisitTransferId' :: EntityFieldWrapper (Entity User) HospitalVisitTransfer HospitalVisitTransferId
hospitalVisitTransferId' = EntityFieldWrapper HospitalVisitTransferId

{-@ measure hospitalVisitTransferSet :: HospitalVisitTransfer -> InformationTransferSetId @-}

{-@ measure hospitalVisitTransferSetCap :: Entity HospitalVisitTransfer -> Bool @-}

{-@ assume hospitalVisitTransferSet' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitTransferSet (entityVal row)},
                          {\field row -> field == hospitalVisitTransferSet (entityVal row)},
                          {\old -> hospitalVisitTransferSetCap old},
                          {\old _ _ -> hospitalVisitTransferSetCap old}>
                          (Entity User) HospitalVisitTransfer InformationTransferSetId
  @-}
hospitalVisitTransferSet' :: EntityFieldWrapper (Entity User) HospitalVisitTransfer InformationTransferSetId
hospitalVisitTransferSet' = EntityFieldWrapper HospitalVisitTransferSet

{-@ measure hospitalVisitTransferVisit :: HospitalVisitTransfer -> HospitalVisitId @-}

{-@ measure hospitalVisitTransferVisitCap :: Entity HospitalVisitTransfer -> Bool @-}

{-@ assume hospitalVisitTransferVisit' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == hospitalVisitTransferVisit (entityVal row)},
                          {\field row -> field == hospitalVisitTransferVisit (entityVal row)},
                          {\old -> hospitalVisitTransferVisitCap old},
                          {\old _ _ -> hospitalVisitTransferVisitCap old}>
                          (Entity User) HospitalVisitTransfer HospitalVisitId
  @-}
hospitalVisitTransferVisit' :: EntityFieldWrapper (Entity User) HospitalVisitTransfer HospitalVisitId
hospitalVisitTransferVisit' = EntityFieldWrapper HospitalVisitTransferVisit

-- * BusinessAssociateAgreement
{-@ mkBusinessAssociateAgreement ::
        x_0: BusinessAssociateId
     -> x_1: CoveredEntityId
     -> x_2: InformationTransferSetId
     -> x_3: Maybe String
     -> StormRecord <{\row -> businessAssociateAgreementBusinessAssociate (entityVal row) == x_0 && businessAssociateAgreementCoveredEntity (entityVal row) == x_1 && businessAssociateAgreementSharedInformation (entityVal row) == x_2 && businessAssociateAgreementPurpose (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (businessAssociateAgreementCoveredEntity (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || businessAssociateAgreementBusinessAssociate (entityVal x_0) == fromJust (userAssociate (entityVal x_1)))}>
                     (Entity User) BusinessAssociateAgreement
  @-}
mkBusinessAssociateAgreement :: BusinessAssociateId -> CoveredEntityId -> InformationTransferSetId -> Maybe String -> StormRecord (Entity User) BusinessAssociateAgreement
mkBusinessAssociateAgreement x_0 x_1 x_2 x_3 = StormRecord (BusinessAssociateAgreement x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity BusinessAssociateAgreement | v == getJust (entityKey v)} @-}



{-@ assume businessAssociateAgreementId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) BusinessAssociateAgreement BusinessAssociateAgreementId
  @-}
businessAssociateAgreementId' :: EntityFieldWrapper (Entity User) BusinessAssociateAgreement BusinessAssociateAgreementId
businessAssociateAgreementId' = EntityFieldWrapper BusinessAssociateAgreementId

{-@ measure businessAssociateAgreementBusinessAssociate :: BusinessAssociateAgreement -> BusinessAssociateId @-}

{-@ measure businessAssociateAgreementBusinessAssociateCap :: Entity BusinessAssociateAgreement -> Bool @-}

{-@ assume businessAssociateAgreementBusinessAssociate' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == businessAssociateAgreementBusinessAssociate (entityVal row)},
                          {\field row -> field == businessAssociateAgreementBusinessAssociate (entityVal row)},
                          {\old -> businessAssociateAgreementBusinessAssociateCap old},
                          {\old _ _ -> businessAssociateAgreementBusinessAssociateCap old}>
                          (Entity User) BusinessAssociateAgreement BusinessAssociateId
  @-}
businessAssociateAgreementBusinessAssociate' :: EntityFieldWrapper (Entity User) BusinessAssociateAgreement BusinessAssociateId
businessAssociateAgreementBusinessAssociate' = EntityFieldWrapper BusinessAssociateAgreementBusinessAssociate

{-@ measure businessAssociateAgreementCoveredEntity :: BusinessAssociateAgreement -> CoveredEntityId @-}

{-@ measure businessAssociateAgreementCoveredEntityCap :: Entity BusinessAssociateAgreement -> Bool @-}

{-@ assume businessAssociateAgreementCoveredEntity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == businessAssociateAgreementCoveredEntity (entityVal row)},
                          {\field row -> field == businessAssociateAgreementCoveredEntity (entityVal row)},
                          {\old -> businessAssociateAgreementCoveredEntityCap old},
                          {\old _ _ -> businessAssociateAgreementCoveredEntityCap old}>
                          (Entity User) BusinessAssociateAgreement CoveredEntityId
  @-}
businessAssociateAgreementCoveredEntity' :: EntityFieldWrapper (Entity User) BusinessAssociateAgreement CoveredEntityId
businessAssociateAgreementCoveredEntity' = EntityFieldWrapper BusinessAssociateAgreementCoveredEntity

{-@ measure businessAssociateAgreementSharedInformation :: BusinessAssociateAgreement -> InformationTransferSetId @-}

{-@ measure businessAssociateAgreementSharedInformationCap :: Entity BusinessAssociateAgreement -> Bool @-}

{-@ assume businessAssociateAgreementSharedInformation' ::
      EntityFieldWrapper <{\x_0 x_1 -> (businessAssociateAgreementCoveredEntity (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || businessAssociateAgreementBusinessAssociate (entityVal x_0) == fromJust (userAssociate (entityVal x_1)))},
                          {\row field -> field == businessAssociateAgreementSharedInformation (entityVal row)},
                          {\field row -> field == businessAssociateAgreementSharedInformation (entityVal row)},
                          {\old -> businessAssociateAgreementSharedInformationCap old},
                          {\old _ _ -> businessAssociateAgreementSharedInformationCap old}>
                          (Entity User) BusinessAssociateAgreement InformationTransferSetId
  @-}
businessAssociateAgreementSharedInformation' :: EntityFieldWrapper (Entity User) BusinessAssociateAgreement InformationTransferSetId
businessAssociateAgreementSharedInformation' = EntityFieldWrapper BusinessAssociateAgreementSharedInformation

{-@ measure businessAssociateAgreementPurpose :: BusinessAssociateAgreement -> (Maybe String) @-}

{-@ measure businessAssociateAgreementPurposeCap :: Entity BusinessAssociateAgreement -> Bool @-}

{-@ assume businessAssociateAgreementPurpose' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == businessAssociateAgreementPurpose (entityVal row)},
                          {\field row -> field == businessAssociateAgreementPurpose (entityVal row)},
                          {\old -> businessAssociateAgreementPurposeCap old},
                          {\old _ _ -> businessAssociateAgreementPurposeCap old}>
                          (Entity User) BusinessAssociateAgreement (Maybe String)
  @-}
businessAssociateAgreementPurpose' :: EntityFieldWrapper (Entity User) BusinessAssociateAgreement (Maybe String)
businessAssociateAgreementPurpose' = EntityFieldWrapper BusinessAssociateAgreementPurpose

-- * Transaction
{-@ mkTransaction ::
        x_0: String
     -> x_1: CoveredEntityId
     -> x_2: CoveredEntityId
     -> x_3: InformationTransferSetId
     -> x_4: String
     -> x_5: String
     -> x_6: String
     -> StormRecord <{\row -> transactionStandard (entityVal row) == x_0 && transactionFirstParty (entityVal row) == x_1 && transactionSecondParty (entityVal row) == x_2 && transactionSharedInformation (entityVal row) == x_3 && transactionDateRequested (entityVal row) == x_4 && transactionDateResponded (entityVal row) == x_5 && transactionPurpose (entityVal row) == x_6},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (userProfileType (entityVal x_1) == 2 || transactionFirstParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || transactionSecondParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)))}>
                     (Entity User) Transaction
  @-}
mkTransaction :: String -> CoveredEntityId -> CoveredEntityId -> InformationTransferSetId -> String -> String -> String -> StormRecord (Entity User) Transaction
mkTransaction x_0 x_1 x_2 x_3 x_4 x_5 x_6 = StormRecord (Transaction x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity Transaction | v == getJust (entityKey v)} @-}



{-@ assume transactionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Transaction TransactionId
  @-}
transactionId' :: EntityFieldWrapper (Entity User) Transaction TransactionId
transactionId' = EntityFieldWrapper TransactionId

{-@ measure transactionStandard :: Transaction -> String @-}

{-@ measure transactionStandardCap :: Entity Transaction -> Bool @-}

{-@ assume transactionStandard' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 2 || transactionFirstParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || transactionSecondParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)))},
                          {\row field -> field == transactionStandard (entityVal row)},
                          {\field row -> field == transactionStandard (entityVal row)},
                          {\old -> transactionStandardCap old},
                          {\old _ _ -> transactionStandardCap old}>
                          (Entity User) Transaction String
  @-}
transactionStandard' :: EntityFieldWrapper (Entity User) Transaction String
transactionStandard' = EntityFieldWrapper TransactionStandard

{-@ measure transactionFirstParty :: Transaction -> CoveredEntityId @-}

{-@ measure transactionFirstPartyCap :: Entity Transaction -> Bool @-}

{-@ assume transactionFirstParty' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 2 || transactionFirstParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || transactionSecondParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)))},
                          {\row field -> field == transactionFirstParty (entityVal row)},
                          {\field row -> field == transactionFirstParty (entityVal row)},
                          {\old -> transactionFirstPartyCap old},
                          {\old _ _ -> transactionFirstPartyCap old}>
                          (Entity User) Transaction CoveredEntityId
  @-}
transactionFirstParty' :: EntityFieldWrapper (Entity User) Transaction CoveredEntityId
transactionFirstParty' = EntityFieldWrapper TransactionFirstParty

{-@ measure transactionSecondParty :: Transaction -> CoveredEntityId @-}

{-@ measure transactionSecondPartyCap :: Entity Transaction -> Bool @-}

{-@ assume transactionSecondParty' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 2 || transactionFirstParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || transactionSecondParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)))},
                          {\row field -> field == transactionSecondParty (entityVal row)},
                          {\field row -> field == transactionSecondParty (entityVal row)},
                          {\old -> transactionSecondPartyCap old},
                          {\old _ _ -> transactionSecondPartyCap old}>
                          (Entity User) Transaction CoveredEntityId
  @-}
transactionSecondParty' :: EntityFieldWrapper (Entity User) Transaction CoveredEntityId
transactionSecondParty' = EntityFieldWrapper TransactionSecondParty

{-@ measure transactionSharedInformation :: Transaction -> InformationTransferSetId @-}

{-@ measure transactionSharedInformationCap :: Entity Transaction -> Bool @-}

{-@ assume transactionSharedInformation' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == transactionSharedInformation (entityVal row)},
                          {\field row -> field == transactionSharedInformation (entityVal row)},
                          {\old -> transactionSharedInformationCap old},
                          {\old _ _ -> transactionSharedInformationCap old}>
                          (Entity User) Transaction InformationTransferSetId
  @-}
transactionSharedInformation' :: EntityFieldWrapper (Entity User) Transaction InformationTransferSetId
transactionSharedInformation' = EntityFieldWrapper TransactionSharedInformation

{-@ measure transactionDateRequested :: Transaction -> String @-}

{-@ measure transactionDateRequestedCap :: Entity Transaction -> Bool @-}

{-@ assume transactionDateRequested' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == transactionDateRequested (entityVal row)},
                          {\field row -> field == transactionDateRequested (entityVal row)},
                          {\old -> transactionDateRequestedCap old},
                          {\old _ _ -> transactionDateRequestedCap old}>
                          (Entity User) Transaction String
  @-}
transactionDateRequested' :: EntityFieldWrapper (Entity User) Transaction String
transactionDateRequested' = EntityFieldWrapper TransactionDateRequested

{-@ measure transactionDateResponded :: Transaction -> String @-}

{-@ measure transactionDateRespondedCap :: Entity Transaction -> Bool @-}

{-@ assume transactionDateResponded' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == transactionDateResponded (entityVal row)},
                          {\field row -> field == transactionDateResponded (entityVal row)},
                          {\old -> transactionDateRespondedCap old},
                          {\old _ _ -> transactionDateRespondedCap old}>
                          (Entity User) Transaction String
  @-}
transactionDateResponded' :: EntityFieldWrapper (Entity User) Transaction String
transactionDateResponded' = EntityFieldWrapper TransactionDateResponded

{-@ measure transactionPurpose :: Transaction -> String @-}

{-@ measure transactionPurposeCap :: Entity Transaction -> Bool @-}

{-@ assume transactionPurpose' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userProfileType (entityVal x_1) == 2 || transactionFirstParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)) || transactionSecondParty (entityVal x_0) == fromJust (userEntity (entityVal x_1)))},
                          {\row field -> field == transactionPurpose (entityVal row)},
                          {\field row -> field == transactionPurpose (entityVal row)},
                          {\old -> transactionPurposeCap old},
                          {\old _ _ -> transactionPurposeCap old}>
                          (Entity User) Transaction String
  @-}
transactionPurpose' :: EntityFieldWrapper (Entity User) Transaction String
transactionPurpose' = EntityFieldWrapper TransactionPurpose

-- * PersonalRepresentative
{-@ mkPersonalRepresentative ::
        x_0: IndividualId
     -> x_1: IndividualId
     -> x_2: Bool
     -> StormRecord <{\row -> personalRepresentativeDependent (entityVal row) == x_0 && personalRepresentativeRepresentative (entityVal row) == x_1 && personalRepresentativeParent (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) PersonalRepresentative
  @-}
mkPersonalRepresentative :: IndividualId -> IndividualId -> Bool -> StormRecord (Entity User) PersonalRepresentative
mkPersonalRepresentative x_0 x_1 x_2 = StormRecord (PersonalRepresentative x_0 x_1 x_2)

{-@ invariant {v: Entity PersonalRepresentative | v == getJust (entityKey v)} @-}



{-@ assume personalRepresentativeId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) PersonalRepresentative PersonalRepresentativeId
  @-}
personalRepresentativeId' :: EntityFieldWrapper (Entity User) PersonalRepresentative PersonalRepresentativeId
personalRepresentativeId' = EntityFieldWrapper PersonalRepresentativeId

{-@ measure personalRepresentativeDependent :: PersonalRepresentative -> IndividualId @-}

{-@ measure personalRepresentativeDependentCap :: Entity PersonalRepresentative -> Bool @-}

{-@ assume personalRepresentativeDependent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == personalRepresentativeDependent (entityVal row)},
                          {\field row -> field == personalRepresentativeDependent (entityVal row)},
                          {\old -> personalRepresentativeDependentCap old},
                          {\old _ _ -> personalRepresentativeDependentCap old}>
                          (Entity User) PersonalRepresentative IndividualId
  @-}
personalRepresentativeDependent' :: EntityFieldWrapper (Entity User) PersonalRepresentative IndividualId
personalRepresentativeDependent' = EntityFieldWrapper PersonalRepresentativeDependent

{-@ measure personalRepresentativeRepresentative :: PersonalRepresentative -> IndividualId @-}

{-@ measure personalRepresentativeRepresentativeCap :: Entity PersonalRepresentative -> Bool @-}

{-@ assume personalRepresentativeRepresentative' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == personalRepresentativeRepresentative (entityVal row)},
                          {\field row -> field == personalRepresentativeRepresentative (entityVal row)},
                          {\old -> personalRepresentativeRepresentativeCap old},
                          {\old _ _ -> personalRepresentativeRepresentativeCap old}>
                          (Entity User) PersonalRepresentative IndividualId
  @-}
personalRepresentativeRepresentative' :: EntityFieldWrapper (Entity User) PersonalRepresentative IndividualId
personalRepresentativeRepresentative' = EntityFieldWrapper PersonalRepresentativeRepresentative

{-@ measure personalRepresentativeParent :: PersonalRepresentative -> Bool @-}

{-@ measure personalRepresentativeParentCap :: Entity PersonalRepresentative -> Bool @-}

{-@ assume personalRepresentativeParent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == personalRepresentativeParent (entityVal row)},
                          {\field row -> field == personalRepresentativeParent (entityVal row)},
                          {\old -> personalRepresentativeParentCap old},
                          {\old _ _ -> personalRepresentativeParentCap old}>
                          (Entity User) PersonalRepresentative Bool
  @-}
personalRepresentativeParent' :: EntityFieldWrapper (Entity User) PersonalRepresentative Bool
personalRepresentativeParent' = EntityFieldWrapper PersonalRepresentativeParent
