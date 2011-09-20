using System.Collections.Generic;
using Com.Lmax.Api.Internal.Events;

namespace Com.Lmax.Api.Internal.Protocol
{
    public class ExecutionEventHandler : DefaultHandler
    {
        private const string RootNodeName = "executions";
        private const string ExecutionNodeName = "execution";
        private const string ExecutionIdNodeName = "executionId";
        private const string PriceNodeName = "price";
        private const string QuantityNodeName = "quantity";
        private const string OrderCancelledNodeName = "orderCancelled";

        private readonly IList<ExecutionBuilder> _executionBuilders;
        private ExecutionBuilder _executionBuilder = new ExecutionBuilder();

        public ExecutionEventHandler() : base(RootNodeName)
        {
            AddHandler(ExecutionIdNodeName);
            AddHandler(PriceNodeName);
            AddHandler(QuantityNodeName);
            _executionBuilders = new List<ExecutionBuilder>();
        }

        public override void EndElement(string endElement)
        {
            if (!ExecutionNodeName.Equals(endElement) && !OrderCancelledNodeName.Equals(endElement)) return;
            
            decimal quantity;
            TryGetValue(QuantityNodeName, out quantity);
            if (ExecutionNodeName.Equals(endElement))
            {
                decimal price;
             
                TryGetValue(PriceNodeName, out price);
                    
                _executionBuilder.Price(price);
                _executionBuilder.Quantity(quantity);
            }
            else if (OrderCancelledNodeName.Equals(endElement))
            {
                _executionBuilder.CancelledQuantity(quantity);
            }

            long executionId;
            TryGetValue(ExecutionIdNodeName, out executionId);
            _executionBuilder.ExecutionId(executionId);
            _executionBuilders.Add(_executionBuilder);
            _executionBuilder = new ExecutionBuilder();
        }

        public IList<ExecutionBuilder> GetExecutionBuilders()
        {
            return _executionBuilders;
        }

        public void Clear()
        {
            _executionBuilders.Clear();
        }
    }
}