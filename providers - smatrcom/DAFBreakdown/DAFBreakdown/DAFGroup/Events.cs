using System;

namespace DAFBreakdown.DAFGroup
{
    public class PortfolioEventArgs : EventArgs
    {
        public string Name;
        public double Cash;
        public double Fee;

        public PortfolioEventArgs(string name, double cash, double fee)
        {
            Name = name;
            Cash = cash;
            Fee = fee;
        }
    }
}
