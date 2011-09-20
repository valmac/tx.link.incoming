using System;
using System.Collections.Generic;
using Com.Lmax.Api.Order;

namespace Com.Lmax.Api.Internal.Events
{
    public class OrderBuilder
    {
        private long _accountId;
        private decimal _cancelledQuantity;
        private decimal _filledQuantity;
        private long _instructionId;
        private long _instrumentId;
        private string _orderId;
        private OrderType _orderType;
        private decimal? _price;
        private decimal _quantity;
        private readonly IDictionary<string, OrderType> _orderTypes = new Dictionary<string, OrderType>();
        private decimal? _stopReferencePrice; 
        private decimal? _stopProfitOffset;
        private decimal? _stopLossOffset;

        public OrderBuilder()
        {
            _orderTypes.Add("STOP_COMPOUND_PRICE_LIMIT", Order.OrderType.LIMIT);
            _orderTypes.Add("PRICE_LIMIT", Order.OrderType.LIMIT);
            _orderTypes.Add("STOP_COMPOUND_MARKET", Order.OrderType.MARKET);
            _orderTypes.Add("MARKET_ORDER", Order.OrderType.MARKET);
            _orderTypes.Add("STOP_LOSS_ORDER", Order.OrderType.STOP_LOSS_MARKET_ORDER);
            _orderTypes.Add("STOP_PROFIT_ORDER", Order.OrderType.STOP_PROFIT_LIMIT_ORDER);
        }

        public Order.Order NewInstance()
        {
            return new Order.Order(_instructionId, _orderId, _instrumentId, _accountId, _price, _quantity, _filledQuantity, _cancelledQuantity, _orderType, 
                                   _stopLossOffset, _stopProfitOffset, _stopReferencePrice);
        }

        public OrderBuilder InstructionId(long anInstructionId)
        {
            _instructionId = anInstructionId;
            return this;
        }

        public OrderBuilder OrderId(string anOrderId)
        {
            _orderId = anOrderId;
            return this;
        }

        public OrderBuilder InstrumentId(long anInstrumentId)
        {
            _instrumentId = anInstrumentId;
            return this;
        }

        public OrderBuilder AccountId(long anAccountId)
        {
            _accountId = anAccountId;
            return this;
        }

        public OrderBuilder Quantity(decimal aQuantity)
        {
            _quantity = aQuantity;
            return this;
        }

        public OrderBuilder FilledQuantity(decimal aFilledQuantity)
        {
            _filledQuantity = aFilledQuantity;
            return this;
        }

        public OrderBuilder Price(decimal aPrice)
        {
            _price = aPrice;
            return this;
        }

        public OrderBuilder OrderType(OrderType anOrderType)
        {
            _orderType = anOrderType;
            return this;
        }

        public OrderBuilder StopReferencePrice(decimal stopReferencePrice)
        {
            _stopReferencePrice = stopReferencePrice;
            return this;
        }

        public OrderBuilder StopProfitOffset(decimal stopProfitOffset)
        {
            _stopProfitOffset = stopProfitOffset;
            return this;
        }
        public OrderBuilder StopLossOffset(decimal stopLossOffset)
        {
            _stopLossOffset = stopLossOffset;
            return this;
        }

        public OrderBuilder OrderType(string messageOrderType)
        {
            OrderType anOrderType;
            if (_orderTypes.TryGetValue(messageOrderType, out anOrderType))
            {
                OrderType(anOrderType);
            }
            else
            {
                try
                {
                    OrderType((OrderType)Enum.Parse(typeof(OrderType), messageOrderType));
                }
                catch (ArgumentException)
                {
                    OrderType(Order.OrderType.UNKNOWN);
                }
            }
            return this;
        }

        public OrderBuilder CancelledQuantity(decimal aCancelledQuantity)
        {
            _cancelledQuantity = aCancelledQuantity;
            return this;
        }
    }
}