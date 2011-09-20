using System;

namespace Com.Lmax.Api.Internal
{
    public class DateTimeUtil
    {
        private static readonly DateTime UnixEpochStart = new DateTime(1970, 1, 1);

        public static long DateTimeToMillis(DateTime dateTime)
        {
            return (long) (dateTime - UnixEpochStart).TotalMilliseconds;
        }
    }
}
