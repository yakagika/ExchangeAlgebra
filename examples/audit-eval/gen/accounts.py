"""Static account catalog copied from EA AccountTitles for generator use.

The pandas oracle intentionally does not import or execute ExchangeAlgebra.
This catalog is a local, static mirror of the constructor names and divisions
documented in src/ExchangeAlgebra/Algebra/Base/Element.hs.
"""

from __future__ import annotations

from collections.abc import Iterable


ACCOUNT_DIVISIONS: dict[str, str] = {
    "Cash": "asset",
    "Deposits": "asset",
    "CurrentDeposits": "asset",
    "Securities": "asset",
    "InvestmentSecurities": "asset",
    "InvestmentInAssociate": "asset",
    "LongTermNationalBonds": "asset",
    "ShortTermNationalBonds": "asset",
    "Products": "asset",
    "Machinery": "asset",
    "Building": "asset",
    "Vehicle": "asset",
    "StockInvestment": "asset",
    "EquipmentInvestment": "asset",
    "LongTermLoansReceivable": "asset",
    "AccountsReceivable": "asset",
    "ShortTermLoansReceivable": "asset",
    "ReserveDepositReceivable": "asset",
    "Gold": "asset",
    "GovernmentService": "asset",
    "CapitalStock": "equity",
    "RetainedEarnings": "equity",
    "LongTermLoansPayable": "liability",
    "ShortTermLoansPayable": "liability",
    "LoansPayable": "liability",
    "ReserveForDepreciation": "liability",
    "DepositPayable": "liability",
    "LongTermNationalBondsPayable": "liability",
    "ShortTermNationalBondsPayable": "liability",
    "ReserveDepositPayable": "liability",
    "CentralBankNotePayable": "liability",
    "Depreciation": "expense",
    "AmortizationExpense": "expense",
    "SalesCost": "expense",
    "BusinessTrip": "expense",
    "Commutation": "expense",
    "UtilitiesExpense": "expense",
    "RentExpense": "expense",
    "AdvertisingExpense": "expense",
    "DeliveryExpenses": "expense",
    "SuppliesExpenses": "expense",
    "MiscellaneousExpenses": "expense",
    "WageExpenditure": "expense",
    "InterestExpense": "expense",
    "TaxesExpense": "expense",
    "ConsumptionExpenditure": "expense",
    "SubsidyExpense": "expense",
    "CentralBankPaymentExpense": "expense",
    "Purchases": "expense",
    "NetIncome": "expense",
    "ValueAdded": "revenue",
    "SubsidyIncome": "revenue",
    "NationalBondInterestEarned": "revenue",
    "DepositInterestEarned": "revenue",
    "GrossProfit": "revenue",
    "OrdinaryProfit": "revenue",
    "InterestEarned": "revenue",
    "ReceiptFee": "revenue",
    "RentalIncome": "revenue",
    "WageEarned": "revenue",
    "TaxesRevenue": "revenue",
    "CentralBankPaymentIncome": "revenue",
    "Sales": "revenue",
    "EquityInEarningsOfInvestee": "revenue",
    "NetLoss": "revenue",
    "PettyCash": "asset",
    "NotesReceivable": "asset",
    "ElectronicallyRecordedReceivable": "asset",
    "CreditCardReceivable": "asset",
    "NotesLoansReceivable": "asset",
    "MerchandiseInventory": "asset",
    "AdvancesPaid": "asset",
    "PrepaidExpenses": "asset",
    "AccruedRevenue": "asset",
    "OtherReceivables": "asset",
    "PaymentsOnBehalf": "asset",
    "SuspensePayments": "asset",
    "ConsumptionTaxPaid": "asset",
    "PrepaidCorporateIncomeTaxes": "asset",
    "Land": "asset",
    "Fixtures": "asset",
    "Patent": "asset",
    "Trademark": "asset",
    "Software": "asset",
    "CashOverShort": "asset",
    "AccountsPayable": "liability",
    "NotesPayable": "liability",
    "ElectronicallyRecordedObligations": "liability",
    "NotesLoansPayable": "liability",
    "BankOverdraft": "liability",
    "AdvancesReceived": "liability",
    "UnearnedRevenue": "liability",
    "AccruedExpenses": "liability",
    "OtherPayables": "liability",
    "DepositsReceived": "liability",
    "SuspenseReceipts": "liability",
    "ConsumptionTaxReceived": "liability",
    "AccruedConsumptionTax": "liability",
    "AccruedCorporateIncomeTaxes": "liability",
    "UnpaidDividends": "liability",
    "AllowanceForDoubtfulAccounts": "asset",   # contra asset (Definition 7 amendment)
    "AccumulatedDepreciation": "asset",        # contra asset (Definition 7 amendment)
    "LegalRetainedEarnings": "equity",
    "CumulativeTranslationAdjustment": "equity",
    "ProvisionForDoubtfulAccounts": "expense",
    "BadDebtLoss": "expense",
    "LossOnSalesOfFixedAssets": "expense",
    "LossOnSalesOfNotesReceivable": "expense",
    "PaymentFees": "expense",
    "MiscellaneousLoss": "expense",
    "CorporateIncomeTaxes": "expense",
    "CommunicationExpenses": "expense",
    "GainOnSalesOfFixedAssets": "revenue",
    "RecoveryOfBadDebts": "revenue",
    "MiscellaneousIncome": "revenue",
    "ReversalOfAllowanceForDoubtfulAccounts": "revenue",
}

CANONICAL_ACCOUNT_TITLES: tuple[str, ...] = tuple(ACCOUNT_DIVISIONS)
DEBIT_NORMAL_CATEGORIES = {"asset", "expense"}
CREDIT_NORMAL_CATEGORIES = {"liability", "equity", "revenue"}

# Contra accounts: home side and financial-statement contribution are the
# reverse of their division's defaults (mirror of EA's `isContra`; the EA
# oracle DeriveEA.hs must agree). Their balances stay non-negative on the
# credit side and are SUBTRACTED inside their division's total.
CONTRA_ACCOUNTS = frozenset({
    "AllowanceForDoubtfulAccounts",
    "AccumulatedDepreciation",
})


def is_known_account(account: str) -> bool:
    return account in ACCOUNT_DIVISIONS


def account_category(account: str) -> str:
    try:
        return ACCOUNT_DIVISIONS[account]
    except KeyError as exc:
        raise KeyError(f"unknown account title: {account}") from exc


def is_contra(account: str) -> bool:
    return account in CONTRA_ACCOUNTS


def normal_side(account: str) -> str:
    category = account_category(account)
    if category in DEBIT_NORMAL_CATEGORIES:
        side = "debit"
    elif category in CREDIT_NORMAL_CATEGORIES:
        side = "credit"
    else:
        raise ValueError(f"unknown account category for {account}: {category}")
    if is_contra(account):
        side = "credit" if side == "debit" else "debit"
    return side


def is_nominal(account: str) -> bool:
    return account_category(account) in {"expense", "revenue"}


def identity_ea_map(accounts: Iterable[str]) -> dict[str, str]:
    return {account: account for account in accounts}


def chart_accounts_from_postings(postings: Iterable[dict]) -> list[str]:
    accounts = {str(posting["account"]) for posting in postings}
    return sorted(accounts)


def account_category_map(accounts: Iterable[str]) -> dict[str, str]:
    return {account: ACCOUNT_DIVISIONS[account] for account in accounts if account in ACCOUNT_DIVISIONS}


def hallucinated_name(seed: int, index: int) -> str:
    return f"PhantomLedgerAccount_{seed}_{index}"

