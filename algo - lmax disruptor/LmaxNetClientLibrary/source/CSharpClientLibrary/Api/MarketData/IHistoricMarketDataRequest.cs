
namespace Com.Lmax.Api.MarketData
{
    ///<summary>
    /// Marker interface for the different types of requests for historic market data.
    ///</summary>
    public interface IHistoricMarketDataRequest : IRequest
    {
    }

    ///<summary>
    /// The format the historic market data is returned in.
    ///</summary>
    public enum Format
    {
        ///<summary>
        /// Comma Separated Values, not a fixed format, the columns will be different according to the different types of data returned.
        ///</summary>
        Csv
    }

    ///<summary>
    /// The time period the data will be aggregated over.
    ///</summary>
    public enum Resolution
    {
        Minute,
        Day
    }
}