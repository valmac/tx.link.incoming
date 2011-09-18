using System;
using System.Text;
using System.Runtime.InteropServices;
using System.Collections.Generic;


namespace tslight
{

	//================================================================================
	#region MarshalUTF8
	static class MarshalUTF8
	{
		private static UTF8Encoding _utf8;

		//--------------------------------------------------------------------------------
		static MarshalUTF8()
		{
			_utf8 = new UTF8Encoding();
		}

		//--------------------------------------------------------------------------------
		public static IntPtr StringToHGlobalUTF8(String data)
		{
			Byte[] dataEncoded = _utf8.GetBytes(data);

			int size = Marshal.SizeOf(dataEncoded[0]) * dataEncoded.Length;

			IntPtr pData = Marshal.AllocHGlobal(size);

			Marshal.Copy(dataEncoded, 0, pData, dataEncoded.Length);

			return pData;
		}

		//--------------------------------------------------------------------------------
		public static String PtrToStringUTF8(IntPtr pData)
		{
			// this is just to get buffer length in bytes
			String errStr = Marshal.PtrToStringAnsi(pData);
			int length = errStr.Length;

			Byte[] data = new byte[length];
			Marshal.Copy(pData, data, 0, length);

			return _utf8.GetString(data);
		}
		//--------------------------------------------------------------------------------
	}
	#endregion // MarshalUTF8
	//================================================================================
	#region TXmlConnector
	public class TXmlConnector
	{
		const String EX_SETTING_CALLBACK = "Не смог установить функцию обратного вызова";

		private static volatile TXmlConnector _instance;
		private static object syncRoot = new Object();

		public delegate void CallBack(IntPtr pData);
		public CallBack myDelegate;
		
		public IList<string> DataList; // список строк с полученными от сервера данными
		public string debug_str;

		//--------------------------------------------------------------------------------
		//private TXmlConnector()
		public TXmlConnector()
		{
			myDelegate = new CallBack(myCallBack);
			if (!SetCallback(myDelegate))
			{
				throw (new Exception(EX_SETTING_CALLBACK));
			}
			_instance = this;
		}

		//--------------------------------------------------------------------------------
		public static TXmlConnector Instance
		{
			get
			{
				if (_instance == null)
				{
					lock (syncRoot)
					{
						if (_instance == null)
							_instance = new TXmlConnector();
					}
				}

				return _instance;
			}
		}

		//--------------------------------------------------------------------------------
		public void myCallBack(IntPtr pData)
		{
			String data = MarshalUTF8.PtrToStringUTF8(pData);
			FreeMemory(pData);

			//RaiseNewDataEvent(data);
			DataList.Add(data);
		}

		//--------------------------------------------------------------------------------
		public class NewDataEventArgs : EventArgs
		{
			private String _data;
			public NewDataEventArgs(String data)
			{
				_data = data;
			}
			public String data
			{
				get
				{
					return _data;
				}
			}
		}

		//--------------------------------------------------------------------------------
		public event EventHandler<NewDataEventArgs> NewData;
		public void RaiseNewDataEvent(String data)
		{
			EventHandler<NewDataEventArgs> handler = NewData;

			if (handler != null)
			{
				handler(new Object(), new NewDataEventArgs(data));
			}
		}


		//--------------------------------------------------------------------------------
		public String SendCommand(String command)
		{

			IntPtr pData = MarshalUTF8.StringToHGlobalUTF8(command);
			IntPtr pResult = SendCommand(pData);

			String result = MarshalUTF8.PtrToStringUTF8(pResult);

			Marshal.FreeHGlobal(pData);
			FreeMemory(pResult);

			return result;

		}

		//--------------------------------------------------------------------------------
		// файл библиотеки TXmlConnector.dll должен находиться в одной папке с программой
		[DllImport("TXmlConnector.dll", CallingConvention = CallingConvention.Winapi)]
		private static extern bool SetCallback(CallBack pCallback);

		[DllImport("TXmlConnector.dll", CallingConvention = CallingConvention.Winapi)]
		private static extern IntPtr SendCommand(IntPtr pData);

		[DllImport("TXmlConnector.dll", CallingConvention = CallingConvention.Winapi)]
		private static extern bool FreeMemory(IntPtr pData);
		//--------------------------------------------------------------------------------

	}
	#endregion // TXmlConnector
	//================================================================================

}
