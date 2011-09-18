namespace StockSharp.Hydra.Quik
{
	using StockSharp.Quik;

	class HydraQuikTrader : QuikTrader
	{
		public HydraQuikTrader(string path, string ddeServer)
			: base(path, ddeServer)
		{
		}

		public override void StartExport()
		{
			StartExport(SecuritiesTable);
		}

		public override void StopExport()
		{
			StopExport(SecuritiesTable);
		}
	}
}