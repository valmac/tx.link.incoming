using System;

namespace TestAtConsole
{
    /// <summary>
    /// Класс описывающий тик
    /// </summary>
    public class Trade
    {
        private string _id;
        private DateTime _dateTime;
        private string _secId;
        private double _price;
        private int _size;

        public Trade(string id,DateTime dateTime, string secID, double price, int size)
        {
            _id = id;
            _dateTime = dateTime;
            _secId = secID;
            _price = price;
            _size = size;
        }

        public static Trade CreateInstance(object[] data, object[] headers)
        {
            DateTime dt = DateTime.Now, tm = DateTime.Now;
            string secId="", id="";
            double price=0;
            int qty = 0;
            for (int i = 0; i < headers.Length; i++)
            {
                switch(headers[i].ToString())
                {
                    case "TRADENUM":
                        id= data[i].ToString();
                        break;
                    case "TRADEDATE":
                        dt = Convert.ToDateTime(data[i].ToString());
                        break;
                    case "TRADETIME":
                        tm = Convert.ToDateTime(data[i].ToString());
                        break;
                    case "SECCODE":
                        secId = data[i].ToString();
                        break;
                    case "PRICE":
                        price = Convert.ToDouble(data[i].ToString());
                        break;
                    case "QTY":
                        qty = Convert.ToInt32(data[i].ToString());
                        break;
                }
            }

            return new Trade(id,dt.Add(tm.TimeOfDay), secId, price, qty);

        }

        /// <summary>
        /// Номер
        /// </summary>
        public string ID
        {
            get { return _id; }
        }

        /// <summary>
        /// Дата-время
        /// </summary>
        public DateTime DateTime
        {
            get { return _dateTime; }
        }

        /// <summary>
        /// Код инструмента
        /// </summary>
        public string SecID
        {
            get { return _secId; }
        }

        /// <summary>
        /// Цена
        /// </summary>
        public double Price
        {
            get { return _price; }
        }

        /// <summary>
        /// Кол-во (лот)
        /// </summary>
        public int Size
        {
            get { return _size; }
        }

        /// <summary>
        /// Строковое представление
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return string.Format("#{0} {1} {2} @{3} {4}",
                _id, _dateTime.TimeOfDay, _secId, _price, _size);
        }
    }
}