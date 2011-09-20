namespace Com.Lmax.Api.Internal.Protocol
{
    public class HeartbeatEventHandler : DefaultHandler
    {
        private const string RootNodeName = "heartbeat";
        private const string Token = "token";
        public event OnHeartbeatReceivedEvent HeartbeatReceived;

        public HeartbeatEventHandler()
            : base(RootNodeName)
        {
            AddHandler(Token);
        }

        public override void EndElement(string endElement)
        {
            if (HeartbeatReceived != null && endElement.Equals(RootNodeName))
            {
                HeartbeatReceived(GetStringValue("token"));
            }
        }
    }
}