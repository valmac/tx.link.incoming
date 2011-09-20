using System.Collections.Generic;
using Com.Lmax.Api.Account;

namespace Com.Lmax.Api.Internal.Events
{
    class AccountStateBuilder
    {
        private long _accountId;
        private decimal _balance;
        private decimal _availableFunds;
        private decimal _availableToWithdraw;
        private decimal _unrealisedProfitAndLoss;
        private decimal _margin;
        private Dictionary<string, decimal> _wallets;

        public AccountStateBuilder AccountId(long accountId)
        {
            _accountId = accountId;
            return this;
        }

        public AccountStateBuilder Balance(decimal balance)
        {
            _balance = balance;
            return this;
        }

        public AccountStateBuilder AvailableFunds(decimal availableFunds)
        {
            _availableFunds = availableFunds;
            return this;
        }

        public AccountStateBuilder AvailableToWithdraw(decimal availableToWithdraw)
        {
            _availableToWithdraw = availableToWithdraw;
            return this;
        }

        public AccountStateBuilder UnrealisedProfitAndLoss(decimal unrealisedProfitAndLoss)
        {
            _unrealisedProfitAndLoss = unrealisedProfitAndLoss;
            return this;
        }

        public AccountStateBuilder Margin(decimal margin)
        {
            _margin = margin;
            return this;
        }

        public AccountStateBuilder Wallets(Dictionary<string, decimal> wallets)
        {
            _wallets = wallets;
            return this;
        }

        public AccountStateEvent NewInstance()
        {
            return new AccountStateEvent(_accountId, _balance, _availableFunds, _availableToWithdraw, _unrealisedProfitAndLoss, _margin, _wallets);
        }
    }

}
