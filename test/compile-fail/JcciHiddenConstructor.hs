module JcciHiddenConstructor where

import ExchangeAlgebra.Accounting.Account (AccountTitles(GovernmentService))
import ExchangeAlgebra.Accounting.Account.Jcci

title :: JcciTitles
title = JcciTitles GovernmentService
