using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using Com.Lmax.Api.MarketData;

namespace Com.Lmax.Api
{
    internal class HistoricMarketDataRequester
    {
        private ISession _session;

        private void OnLoginSuccess(ISession session)
        {
            const long instructionId = 1;
            const long instrumentId = 4001;

            _session = session;
            _session.HistoricMarketDataReceived += OnHistoricMarketData;
            _session.Subscribe(new HistoricMarketDataSubscriptionRequest(),
                               () => Console.WriteLine("Successful subscription"),
                               failureResponse => Console.Error.WriteLine("Failed to subscribe: {0}", failureResponse));

            _session.RequestHistoricMarketData(new AggregateHistoricMarketDataRequest(instructionId, instrumentId,
                                                                                      DateTime.Parse("2011-05-11"),
                                                                                      DateTime.Parse("2011-06-13"),
                                                                                      Resolution.Day, Format.Csv,
                                                                                      Option.Bid),
                                               () => Console.WriteLine("Successful request"),
                                               failureResponse =>
                                               Console.Error.WriteLine("Failed request: {0}", failureResponse));
            _session.Start();
        }

        private void OnHistoricMarketData(long instructionId, List<Uri> uris)
        {
            foreach (var uri in uris)
            {
                _session.OpenUri(uri, OnUriResponse, FailureCallback("open uri"));
            }
        }

        private static void OnUriResponse(Uri uri, BinaryReader reader)
        {
            using (var stream = new GZipStream(reader.BaseStream, CompressionMode.Decompress))
            {
                const int size = 1024;
                var buffer = new byte[size];
                var numBytes = stream.Read(buffer, 0, size);
                while (numBytes > 0)
                {
                    Console.Write(Encoding.UTF8.GetString(buffer, 0, numBytes));
                    numBytes = stream.Read(buffer, 0, size);
                }
            }
        }

        private static OnFailure FailureCallback(string failedFunction)
        {
            return
                failureResponse =>
                Console.Error.WriteLine("Failed to " + failedFunction + " due to: " + failureResponse.Message);
        }

        public static void Main(string[] args)
        {
            var loginClient = new HistoricMarketDataRequester();
            if (args.Length != 4)
            {
                Console.WriteLine("Usage " + loginClient.GetType().Name +
                                  " <url> <username> <password> [CFD_DEMO|CFD_LIVE]");
                Environment.Exit(-1);
            }

            var url = args[0];
            var username = args[1];
            var password = args[2];
            var productType = (ProductType) Enum.Parse(typeof (ProductType), args[3].ToUpper());

            var lmaxApi = new LmaxApi(url);

            lmaxApi.Login(new LoginRequest(username, password, productType), loginClient.OnLoginSuccess,
                          FailureCallback("log in"));
        }
    }
}