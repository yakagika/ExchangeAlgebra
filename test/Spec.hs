-- | Assemble the existing suites and the seven domain suites.
module Main (main) where

import qualified Value.MoneyParseSpec as MoneyParseSpec
import qualified Algebra.ExactSumSpec as ExactSumSpec
import qualified Admission.Spec as AdmissionSpec
import qualified Admission.ClosingSpec as AdmissionClosingSpec
import qualified Admission.CatalogSpec as AdmissionCatalogSpec
import qualified Transfer.RuleSpec as TransferRuleSpec
import qualified Algebra.ProjWildcardSpec as ProjWildcardSpec
import qualified Posting.PostingSpec as PostingSpec
import qualified Posting.SettleSpec as SettleSpec
import qualified Journal.CarrySpec as CarrySpec
import qualified Journal.SideTotalsSpec as SideTotalsSpec
import qualified Golden.WireFormat as WireFormat
import qualified Algebra.CoreSpec as CoreSpec
import qualified Journal.JournalSpec as JournalSpec
import qualified Accounting.RegistrySpec as RegistrySpec
import qualified IO.InputOutputSpec as InputOutputSpec
import qualified Simulate.SimulateSpec as SimulateSpec
import qualified Simulation.NetworkSpec as NetworkSpec
import qualified Simulation.OptimizeSpec as OptimizeSpec

main :: IO ()
main = do
    MoneyParseSpec.runTests
    ExactSumSpec.runTests
    AdmissionSpec.runTests
    AdmissionClosingSpec.runTests
    AdmissionCatalogSpec.runTests
    TransferRuleSpec.runTests
    ProjWildcardSpec.runTests
    PostingSpec.runTests
    SettleSpec.runTests
    CarrySpec.runTests
    SideTotalsSpec.runTests
    WireFormat.checkFixture
    CoreSpec.runTests
    JournalSpec.runTests
    RegistrySpec.runTests
    InputOutputSpec.runTests
    SimulateSpec.runTests
    NetworkSpec.runTests
    OptimizeSpec.runTests
