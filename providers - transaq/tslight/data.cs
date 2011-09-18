using System;
using System.Data;


namespace tslight
{

	//================================================================================
	#region DataSet_tslight
	//--------------------------------------------------------------------------------
	public class DataSet_tslight : DataSet
	{
		public DataTable_security t_security;	
		public DataTable_timeframe t_timeframe;	
		public DataTable_candle t_candle;	
		
		//--------------------------------------------------------------------------------
		public DataSet_tslight():
			base()
		{
			t_security = new DataTable_security();
			t_timeframe = new DataTable_timeframe();
			t_candle = new DataTable_candle();
			
		}
		//--------------------------------------------------------------------------------
	}
	//--------------------------------------------------------------------------------
	#endregion
	//================================================================================
	#region DataTable_security
	//--------------------------------------------------------------------------------
	public class DataTable_security : DataTable
	{
		public DataColumn security_id;
		public DataColumn security_name;
		public DataColumn security_code;
		public DataColumn lotsize;
		
		//--------------------------------------------------------------------------------
		public DataTable_security()
		{
			TableName = "t_security";
			BeginInit();
			InitClass();
			EndInit();
		}
		//--------------------------------------------------------------------------------
		public int Count
		{
			get { return Rows.Count; }
		}
		//--------------------------------------------------------------------------------
		public void Add_Row(DataRow_security row)
		{
			Rows.Add(row);
		}
		//--------------------------------------------------------------------------------
		public void Remove_Row(DataRow_security row)
		{
			Rows.Remove(row);
		}
		//--------------------------------------------------------------------------------
		public DataRow_security Add_Row(int security_id, string security_name, string security_code, int lotsize)
		{
			DataRow_security row = (DataRow_security)NewRow();
			object[] aValues = new object[]
			{
				security_id, security_name, security_code, lotsize
			};
			row.ItemArray = aValues;
			Rows.Add(row);
			return row;
		}
		//--------------------------------------------------------------------------------
		public DataRow_security New_Row()
		{
			return (DataRow_security)NewRow();
		}
		//--------------------------------------------------------------------------------
		protected override global::System.Data.DataRow NewRowFromBuilder(global::System.Data.DataRowBuilder builder)
		{
			return new DataRow_security(builder);
		}
		//--------------------------------------------------------------------------------
		public DataRow FindByID(uint id)
		{
			return (DataRow_security)Rows.Find(new object[] {id} );
		}
		//--------------------------------------------------------------------------------
		internal void InitVars()
		{
			security_id = Columns["security_id"];
			security_name = Columns["security_name"];
			security_code = Columns["security_code"];
			lotsize = Columns["lotsize"];
		}
		//--------------------------------------------------------------------------------
		internal void InitClass()
		{
			security_id = new DataColumn("security_id", typeof(int), null, MappingType.Element);
			Columns.Add(security_id);
			security_name = new DataColumn("security_name", typeof(string), null, MappingType.Element);
			Columns.Add(security_name);
			security_code = new DataColumn("security_code", typeof(string), null, MappingType.Element);
			Columns.Add(security_code);
			lotsize = new DataColumn("lotsize", typeof(int), null, MappingType.Element);
			Columns.Add(lotsize);
			
			PrimaryKey = new DataColumn[] { security_id };
			security_name.MaxLength = 50;
			security_code.MaxLength = 50;
		}
		//--------------------------------------------------------------------------------
		protected override Type GetRowType()
		{
			return typeof(DataRow_security);
		}
		//--------------------------------------------------------------------------------
		public DataRow_security this[int index]
		{
			get { return (DataRow_security)Rows[index]; }
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // DataTable_security
	//================================================================================
	#region DataRow_security
	public partial class DataRow_security : global::System.Data.DataRow
	{
		private DataTable_security table;
		
		//--------------------------------------------------------------------------------
		internal DataRow_security(global::System.Data.DataRowBuilder rb):
			base(rb)
		{
			table = (DataTable_security)Table;
		}
		//--------------------------------------------------------------------------------
		public int security_id
		{
			get { return (int)this[table.security_id]; }
			set { this[table.security_id] = value; }
		}
		//--------------------------------------------------------------------------------
		public string security_name
		{
			get { return (string)this[table.security_name]; }
			set { this[table.security_name] = value; }
		}
		//--------------------------------------------------------------------------------
		public string security_code
		{
			get { return (string)this[table.security_code]; }
			set { this[table.security_code] = value; }
		}
		//--------------------------------------------------------------------------------
		public int lotsize
		{
			get { return (int)this[table.lotsize]; }
			set { this[table.lotsize] = value; }
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // DataRow_security
	//================================================================================
	#region DataTable_timeframe
	//--------------------------------------------------------------------------------
	public class DataTable_timeframe : DataTable
	{
		public DataColumn timeframe_id;
		public DataColumn timeframe_length;
		public DataColumn timeframe_name;
		
		//--------------------------------------------------------------------------------
		public DataTable_timeframe()
		{
			TableName = "t_timeframe";
			BeginInit();
			InitClass();
			EndInit();
		}
		//--------------------------------------------------------------------------------
		public int Count
		{
			get { return Rows.Count; }
		}
		//--------------------------------------------------------------------------------
		public void Add_Row(DataRow_timeframe row)
		{
			Rows.Add(row);
		}
		//--------------------------------------------------------------------------------
		public void Remove_Row(DataRow_timeframe row)
		{
			Rows.Remove(row);
		}
		//--------------------------------------------------------------------------------
		public DataRow_timeframe Add_Row(int timeframe_id, int timeframe_length, string timeframe_name)
		{
			DataRow_timeframe row = (DataRow_timeframe)NewRow();
			object[] aValues = new object[]
			{
				timeframe_id, timeframe_length, timeframe_name
			};
			row.ItemArray = aValues;
			Rows.Add(row);
			return row;
		}
		//--------------------------------------------------------------------------------
		public DataRow_timeframe New_Row()
		{
			return (DataRow_timeframe)NewRow();
		}
		//--------------------------------------------------------------------------------
		protected override global::System.Data.DataRow NewRowFromBuilder(global::System.Data.DataRowBuilder builder)
		{
			return new DataRow_timeframe(builder);
		}
		//--------------------------------------------------------------------------------
		public DataRow FindByID(uint id)
		{
			return (DataRow_timeframe)Rows.Find(new object[] {id} );
		}
		//--------------------------------------------------------------------------------
		internal void InitVars()
		{
			timeframe_id = Columns["timeframe_id"];
			timeframe_length = Columns["timeframe_length"];
			timeframe_name = Columns["timeframe_name"];
		}
		//--------------------------------------------------------------------------------
		internal void InitClass()
		{
			timeframe_id = new DataColumn("timeframe_id", typeof(int), null, MappingType.Element);
			Columns.Add(timeframe_id);
			timeframe_length = new DataColumn("timeframe_length", typeof(int), null, MappingType.Element);
			Columns.Add(timeframe_length);
			timeframe_name = new DataColumn("timeframe_name", typeof(string), null, MappingType.Element);
			Columns.Add(timeframe_name);
			
			PrimaryKey = new DataColumn[] { timeframe_id };
			timeframe_name.MaxLength = 50;
		}
		//--------------------------------------------------------------------------------
		protected override Type GetRowType()
		{
			return typeof(DataRow_timeframe);
		}
		//--------------------------------------------------------------------------------
		public DataRow_timeframe this[int index]
		{
			get { return (DataRow_timeframe)Rows[index]; }
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // DataTable_timeframe
	//================================================================================
	#region DataRow_timeframe
	public partial class DataRow_timeframe : global::System.Data.DataRow
	{
		private DataTable_timeframe table;
		
		//--------------------------------------------------------------------------------
		internal DataRow_timeframe(global::System.Data.DataRowBuilder rb):
			base(rb)
		{
			table = (DataTable_timeframe)Table;
		}
		//--------------------------------------------------------------------------------
		public int timeframe_id
		{
			get { return (int)this[table.timeframe_id]; }
			set { this[table.timeframe_id] = value; }
		}
		//--------------------------------------------------------------------------------
		public int timeframe_length
		{
			get { return (int)this[table.timeframe_length]; }
			set { this[table.timeframe_length] = value; }
		}
		//--------------------------------------------------------------------------------
		public string timeframe_name
		{
			get { return (string)this[table.timeframe_name]; }
			set { this[table.timeframe_name] = value; }
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // DataRow_timeframe
	//================================================================================
	#region DataTable_candle
	//--------------------------------------------------------------------------------
	public class DataTable_candle : DataTable
	{
		public DataColumn date;
		public DataColumn open;
		public DataColumn high;
		public DataColumn low;
		public DataColumn close;
		public DataColumn volume;
		
		//--------------------------------------------------------------------------------
		public DataTable_candle()
		{
			TableName = "t_candle";
			BeginInit();
			InitClass();
			EndInit();
		}
		//--------------------------------------------------------------------------------
		public int Count
		{
			get { return Rows.Count; }
		}
		//--------------------------------------------------------------------------------
		public void Add_Row(DataRow_candle row)
		{
			Rows.Add(row);
		}
		//--------------------------------------------------------------------------------
		public void Remove_Row(DataRow_candle row)
		{
			Rows.Remove(row);
		}
		//--------------------------------------------------------------------------------
		public DataRow_candle Add_Row(DateTime date, double open, double high, double low, double close, int volume)
		{
			DataRow_candle row = (DataRow_candle)NewRow();
			object[] aValues = new object[]
			{
				date, open, high, low, close, volume
			};
			row.ItemArray = aValues;
			Rows.Add(row);
			return row;
		}
		//--------------------------------------------------------------------------------
		public DataRow_candle New_Row()
		{
			return (DataRow_candle)NewRow();
		}
		//--------------------------------------------------------------------------------
		protected override global::System.Data.DataRow NewRowFromBuilder(global::System.Data.DataRowBuilder builder)
		{
			return new DataRow_candle(builder);
		}
		//--------------------------------------------------------------------------------
		public DataRow FindByDT(DateTime dt)
		{
			return (DataRow_candle)Rows.Find(new object[] {dt} );
		}
		//--------------------------------------------------------------------------------
		internal void InitVars()
		{
			date = Columns["date"];
			open = Columns["open"];
			high = Columns["high"];
			low = Columns["low"];
			close = Columns["close"];
			volume = Columns["volume"];
		}
		//--------------------------------------------------------------------------------
		internal void InitClass()
		{
			date = new DataColumn("date", typeof(DateTime), null, MappingType.Element);
			Columns.Add(date);
			open = new DataColumn("open", typeof(double), null, MappingType.Element);
			Columns.Add(open);
			high = new DataColumn("high", typeof(double), null, MappingType.Element);
			Columns.Add(high);
			low = new DataColumn("low", typeof(double), null, MappingType.Element);
			Columns.Add(low);
			close = new DataColumn("close", typeof(double), null, MappingType.Element);
			Columns.Add(close);
			volume = new DataColumn("volume", typeof(int), null, MappingType.Element);
			Columns.Add(volume);
			
			//PrimaryKey = new DataColumn[] { date };
		}
		//--------------------------------------------------------------------------------
		protected override Type GetRowType()
		{
			return typeof(DataRow_candle);
		}
		//--------------------------------------------------------------------------------
		public DataRow_candle this[int index]
		{
			get { return (DataRow_candle)Rows[index]; }
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // DataTable_candle
	//================================================================================
	#region DataRow_candle
	public partial class DataRow_candle : global::System.Data.DataRow
	{
		private DataTable_candle table;
		
		//--------------------------------------------------------------------------------
		internal DataRow_candle(global::System.Data.DataRowBuilder rb):
			base(rb)
		{
			table = (DataTable_candle)Table;
		}
		//--------------------------------------------------------------------------------
		public DateTime date
		{
			get { return (DateTime)this[table.date]; }
			set { this[table.date] = value; }
		}
		//--------------------------------------------------------------------------------
		public double open
		{
			get { return (double)this[table.open]; }
			set { this[table.open] = value; }
		}
		//--------------------------------------------------------------------------------
		public double high
		{
			get { return (double)this[table.high]; }
			set { this[table.high] = value; }
		}
		//--------------------------------------------------------------------------------
		public double low
		{
			get { return (double)this[table.low]; }
			set { this[table.low] = value; }
		}
		//--------------------------------------------------------------------------------
		public double close
		{
			get { return (double)this[table.close]; }
			set { this[table.close] = value; }
		}
		//--------------------------------------------------------------------------------
		public int volume
		{
			get { return (int)this[table.volume]; }
			set { this[table.volume] = value; }
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // DataRow_candle
	//================================================================================

}
