using System;
using Com.Lmax.Api.OrderBook;

namespace Com.Lmax.Api.Internal.Protocol
{
    public class OrderBookStatusEventHandler : DefaultHandler
    {
        private const string OrderBookId = "id";
        private const string Status = "status";        

        public OrderBookStatusEventHandler()
            : base("orderBookStatus")
        {  
            AddHandler(OrderBookId);
            AddHandler(Status);
        }

        public override void EndElement(string endElement)
        {
            if (OrderBookStatusChanged != null && ElementName == endElement)
            {
                long instrumentId;
                TryGetValue(OrderBookId, out instrumentId);
                string statusString = GetStringValue(Status);
                OrderBookStatus status = (OrderBookStatus)Enum.Parse(typeof(OrderBookStatus), statusString);
       

                OrderBookStatusChanged(new OrderBookStatusEvent(instrumentId, status));
            }
        }

        public event OnOrderBookStatusEvent OrderBookStatusChanged;        
    }
}

