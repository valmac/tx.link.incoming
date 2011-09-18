#region License

/* -----------------------------------------------------------------------------------------------------------------
 * ОБЩЕЕ ЛИЦЕНЗИОННОЕ СОГЛАШЕНИЕ
 * -----------------------------------------------------------------------------------------------------------------
 * Настоящее Лицензионное соглашение (Лицензия) оговаривает Ваши права на использование представленного программного 	
 * обеспечения ("Программного Обеспечения"), и использование Вами  данного Программного Обеспечения означает, 
 * что Вы принимаете все условия настоящего Лицензионного соглашения в полном объеме.
 * 
 * Все исключительные права, включая авторские права на Программное Обеспечение, если иное не оговорено особо, 
 * принадлежат Александру Александрову, защищены законами и международными соглашениями об авторских правах.
 * 
 * Вы можете использовать Программное Обеспечение, целиком или частично, в любых коммерческих или некоммерческих 
 * целях, включая распространение любого, разработанного на его основе или работающего с его помощью программного 
 * обеспечения (Вторичного Программного Обеспечения).
 * 
 * При этом Вы подтверждаете свое согласие со следующим:
 * 
 *   1. Не удалять, не изменять, сохранять в понятном виде  уведомление об авторских правах и другие уведомления, 
 * содержащиеся в Программном Обеспечении.
 * 
 *   2. Если вы распространяете Программное Обеспечение в форме исходного кода, Вы можете делать это только в 
 * соответствии с настоящей Лицензией, и Вы должны распространять полную копию данной лицензии вместе с Вашим 
 * дистрибутивом. Если вы распространяете Программное Обеспечение исключительно в виде объектного кода, Вы можете 
 * делать это  только по лицензии, требования которой полностью удовлетворяют требованиям настоящей Лицензии.

 *   3. Данное Программное Обеспечение поставляется "как есть", без каких бы то ни было гарантий и обязательств. Это 
 * означает полное отсутствие выраженных, подразумеваемых или предусмотренных законодательством гарантий. Вы  так же 
 * должны установить аналогичный отказ от гарантий, если Вы распространяете Вторичное Программное Обеспечение.
 * 
 *   4. Мы не несем  ответственность за какие-либо убытки и/или ущерб (в том числе, убытки в связи с недополученной 
 * коммерческой прибылью, прерыванием коммерческой или производственной деятельности, утратой деловой информации и 
 * иной имущественный ущерб), возникающие в связи с использованием или невозможностью использования Программного 
 * Обеспечения. Вы  так же должны установить аналогичный отказ от ответственности, если Вы распространяете Вторичное 
 * Программное Обеспечение.
 * 
 *   6. Какие либо патентные права, если таковые предоставляются в настоящей Лицензии, применяются только к 
 * Программному Обеспечению, а не к любому Вторичному Программному Обеспечению.
 *   
 *   7. Ваши права, предоставляемые Вам в соответствии с настоящее Лицензией, автоматически прекращаются, если вы 
 * нарушите настоящую Лицензию каким-либо образом.
 * 
 *   8. Все права, явно не предоставленные Вам настоящей Лицензии -  Защищены. 
 * ---
 * Copyright (c) 2008, 2009. "Tranquikizer" Project. Александр Александров. Все права защищены.
 */
#endregion

#region Used Namespaces

// System Namespases
using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text;
#endregion

namespace TRANS2QUIKSHARP
{
    /// <summary>
    /// Класс предоставляет C# описание функций библиотеки TRANS2QUIK.DLL
    /// </summary>
    public static class QUIKAPI
    {
        #region Member

        /// <summary>
        /// Last Executed Function Name
        /// </summary>
        private static System.String m_LEF = string.Empty;
        /// <summary>
        /// Extended Error Code
        /// </summary>
        private static System.Int32 m_EEC = 0;
        /// <summary>
        /// Error Message
        /// </summary>
        private static System.String m_ERM = string.Empty;
        /// <summary>
        /// Error Message Size
        /// </summary>
        private static System.UInt32 m_EMS = 0;
        /// <summary>
        /// Result Message
        /// </summary>
        private static System.String m_RSM = string.Empty;
        /// <summary>
        /// Reply Code
        /// </summary>
        private static System.Int32 m_RPC = 0;
        /// <summary>
        /// TransactionID
        /// </summary>
        private static System.UInt32 m_TID = 0;
        /// <summary>
        /// Result Message Size
        /// </summary>
        private static System.UInt32 m_RMS = 0;
        #endregion

        #region Public Properties

        /// <summary>
        /// <see cref="String"/>Возвращает название функции (метода) библиотеки выполнявшейся последней (Last Executed Function Name)
        /// </summary>
        public static String LEF
        {
            get { return m_LEF; }
        }

        /// <summary>
        /// <see cref="Int32"/>Возвращает Расширенный код ошибки (Extended Error Code)
        /// </summary>
        public static Int32 EEC
        {
            get { return m_EEC; }
        }

        /// <summary>
        /// Возвращает сообщение об ошибке (Error Message)
        /// </summary>
        public static String ERM
        {
            get { return m_ERM; }
        }

        /// <summary>
        /// <see cref="String"/>Возвращает сообщение описывающее результат выполнения некоторых функций библиотеки (Result Message)
        /// </summary>
        public static String RSM
        {
            get { return m_RSM; }
        }

        /// <summary>
        /// <see cref="Int32"/>Возвращает код результа метода обратного вызова обработки транзакций (Reply Code)
        /// </summary>
        public static System.Int32 RPC
        {
            get { return m_RPC; }
        }

        /// <summary>
        /// <see cref="UInt32"/>Взвращает идентификатор транзакции (Transaction ID)
        /// </summary>
        public static System.UInt32 TID
        {
            get { return m_TID; }
        }
        #endregion

        #region Prototype Method

        //-----------------------------------------------------------------
        //long TRANS2QUIK_API __stdcall TRANS2QUIK_CONNECT (
        //LPSTR lpstConnectionParamsString, 
        //long* pnExtendedErrorCode, 
        //LPSTR lpstrErrorMessage, 
        //DWORD dwErrorMessageSize);
        //-----------------------------------------------------------------
        /// <summary>
        /// Функция для установления связи библиотеки TRANS2QUIK.DLL с терминалом QUIK
        /// </summary>
        /// <param name="connectionParamsString">Полный путь к каталогу, в котором установлен INFO.EXE, с которым устанавливаем соединениею. Тип <b>String</b></param>
        /// <param type="String"></param>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>соединение установлено успешно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND</term>
        /// <description>в указанном каталоге либо отсутствует INFO.EXE, либо у него не запущен сервис обработки
        /// внешних подключений, в extendedErrorCode в этом случае передается 0</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_VERSION_NOT_SUPPORTED</term>
        /// <description>используемая версия Trans2QUIK.DLL указанным INFO.EXE не поддерживается,
        /// в extendedErrorCode в этом случае передается 0</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_ALREADY_CONNECTED_TO_QUIK</term>
        /// <description>соединение уже установлено, в extendedErrorCode в этом случае передается 0</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>произошла ошибка при установлении соединения, в extendedErrorCode в этом случае
        /// передается дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_CONNECT@16", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 CONNECT(
            System.String connectionParamsString,
            ref System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_IS_DLL_CONNECTED (
        // long* pnExtendedErrorCode, 
        // LPSTR lpstrErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция для проверки соединения библиотеки TRANS2QUIK.DLL с терминалом QUIK
        /// </summary>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_DLL_CONNECTED</term>
        /// <description>соединение библиотеки TRANS2QUIK.DLL с терминалом QUIK установлено</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_NOT_CONNECTED</term>
        /// <description>не установлена связь библиотеки TRANS2QUIK.DLL с терминалом QUIK</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_IS_DLL_CONNECTED@12", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 IS_DLL_CONNECTED(
            ref System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_DISCONNECT (
        // long* pnExtendedErrorCode, 
        // LPSTR lpstrErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция для разрыва соединения библиотеки TRANS2QUIK.DLL с терминалом QUIK
        /// </summary>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>соединение библиотеки TRANS2QUIK.DLL с терминалом QUIK разорвано успешно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>произошла ошибка при разрыве соединения, в extendedErrorCode в этом случае
        /// передается дополнительный код ошибки</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_NOT_CONNECTED</term>
        /// <description>попытка разорвать соединение при не установленной связи. В этом случае в
        /// extendedErrorCode может передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_DISCONNECT@12", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 DISCONNECT(
            ref System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_IS_QUIK_CONNECTED (
        // long* pnExtendedErrorCode, 
        // LPSTR lpstrErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция для проверки соединения терминала QUIK с сервером QUIK
        /// </summary>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_QUIK_CONNECTED</term>
        /// <description>соединение установлено</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_QUIK_NOT_CONNECTED</term>
        /// <description>соединение не установлено</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_NOT_CONNECTED</term>
        /// <description>не установлена связь библиотеки TRANS2QUIK.DLL с терминалом QUIK. В этом случае
        /// проверить наличие или отсутствие связи терминала QUIK с сервером невозможно </description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_IS_QUIK_CONNECTED@12", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 IS_QUIK_CONNECTED(
            ref System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_SEND_SYNC_TRANSACTION (
        // LPSTR lpstTransactionString, 
        // long* pnReplyCode, 
        // PDWORD pdwTransId, 
        // double* pdOrderNum, 
        // LPSTR lpstrResultMessage, 
        // DWORD dwResultMessageSize, 
        // long* pnExtendedErrorCode, 
        // LPSTR lpstErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция для отправки синхронной транзакции
        /// </summary>
        /// <param name="transactionString">Строка с описанием транзакции. Формат строки тот же самый, что и при
        /// отправке транзакций через файл</param>
        /// <param name="replyCode">Получает статус выполнения транзакции. Значения статусов те же самые, что и
        /// при подаче заявок через файл</param>
        /// <param name="transactionID">Получает значение TransID транзакции, указанной пользователем.</param>
        /// <param name="orderNumber">В случае успеха получает номер заявки в торговой системе</param>
        /// <param name="resultMessage">В случае успеха содержит сообщение торговой системы</param>
        /// <param name="resultMessageSize">Содержит длину строки, на которую ссылается указатель resultMessage</param>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>транзакция отправлена успешно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_WRONG_SYNTAX</term>
        /// <description>строка транзакции заполнена неверно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между библиотекой TRANS2QUIK.DLL и терминалом QUIK</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_QUIK_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между терминалом QUIK и сервером</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>транзакцию отправить не удалось. В переменную extendedErrorCode в этом случае может
        /// передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_SEND_SYNC_TRANSACTION@36", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 SEND_SYNC_TRANSACTION(
            System.String transactionString,
            ref System.Int32 replyCode,
            ref System.UInt32 transactionID,
            ref System.Double orderNumber,
            System.String resultMessage,
            System.UInt32 resultMessageSize,
            ref System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_SEND_ASYNC_TRANSACTION (
        // LPSTR lpstTransactionString, 
        // long* pnExtendedErrorCode, 
        // LPSTR lpstErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция для отправки асинхронной транзакции
        /// </summary>
        /// <param name="transactionString">Строка с описанием транзакции. Формат строки такой же, что и при
        /// отправке транзакций через файл</param>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>транзакция отправлена успешно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_WRONG_SYNTAX</term>
        /// <description>строка транзакции заполнена неверно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между библиотекой TRANS2QUIK.DLL и терминалом QUIK</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_QUIK_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между терминалом квик и сервером</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>транзакцию отправить не удалось. В переменную extendedErrorCode в этом случае может
        /// передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_SEND_ASYNC_TRANSACTION@16", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 SEND_ASYNC_TRANSACTION(
            System.String transactionString,
            ref System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // void TRANS2QUIK_CONNECTION_STATUS_CALLBACK (
        // long nConnectionEvent, 
        // long nExtendedErrorCode, 
        // LPSTR lpstrInfoMessage)
        // -----------------------------------------------------------------
        /// <summary>
        /// Описывает прототип функции обратного вызова для контроля за
        /// состоянием соединения между TRANS2QUIK.DLL и используемым терминалом
        /// QUIK и между используемым терминалом QUIK и сервером
        /// </summary>
        /// <param name="connectionEvent">Число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_QUIK_CONNECTED</term>
        /// <description>соединение между терминалом QUIK и сервером установлено</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_QUIK_DISCONNECTED</term>
        /// <description>соединение между терминалом QUIK и сервером разорвано</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_CONNECTED</term>
        /// <description>соединение между DLL и используемым терминалом QUIK установлено</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_DISCONNECTED</term>
        /// <description>соединение между DLL и используемым терминалом QUIK разорвано</description>
        /// </item>
        /// </list>
        /// </param>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="infoMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// 
        public delegate void CONNECTION_STATUS_CALLBACK(
            System.Int32 connectionEvent,
            System.Int32 extendedErrorCode,
            System.String infoMessage);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK (
        // TRANS2QUIK_CONNECTION_STATUS_CALLBACK pfConnectionStatusCallback, 
        // long* pnExtendedErrorCode, 
        // LPSTR lpstrErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция устанавливает функцию обратного вызова ConnectionStatusCallBack
        /// </summary>
        /// <param name="connectionStatusCallBack">Ссылается на функцию, которая будет обрабатывать информацию
        /// о состоянии связи библиотеки TRANS2QUIK.DLL с терминалом QUIK или терминала QUIK с сервером</param>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>функция обратного вызова установлена</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>функцию обратного вызова установить не удалось. В этом случае в переменную 
        /// extendedErrorCode может передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 SET_CONNECTION_STATUS_CALLBACK(
            CONNECTION_STATUS_CALLBACK connectionStatusCallBack,
            System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);

        // -----------------------------------------------------------------
        // void TRANS2QUIK_TRANSACTION_REPLY_CALLBACK(
        // long nTransactionResult, 
        // long nTransactionExtendedErrorCode, 
        // long nTransactionReplyCode, 
        // DWORD dwTransId, 
        // double dOrderNum, 
        // LPSTR lpstrTransactionReplyMessage)
        // -----------------------------------------------------------------
        /// <summary>
        /// Описывает прототип функции обратного вызова для обработки полученной информации об отправленной
        /// транзакции
        /// </summary>
        /// <param name="transactionResult">
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>транзакция передана успешно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_WRONG_SYNTAX</term>
        /// <description>строка транзакции заполнена неверно</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_DLL_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между библиотекой TRANS2QUIK.DLL и терминалом QUIK</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_QUIK_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между терминалом квик и сервером</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>транзакцию передать не удалось. В этом случае в переменную extendedErrorCode может
        /// передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </param>
        /// <param name="transactionExtendedErrorCode">В случае возникновения проблемы при выходе из функции
        /// обратного вызова в переменную может быть помещен расширенный код ошибки</param>
        /// <param name="transactionReplyCode">Cтатус выполнения транзакции. Значения
        /// статусов те же самые, что и при подаче заявок через файл</param>
        /// <param name="transactionID">Содержимое параметра TransId, который получила зарегестрированная транзакция</param>
        /// <param name="orderNumber">Номер транзакции, присвоенный торговой системой</param>
        /// <param name="transactionReplyMessage">Сообщение от торговой системы или сервера QUIK</param>
        /// 
        public delegate void TRANSACTION_REPLY_CALLBACK(
            System.Int32 transactionResult,
            System.Int32 transactionExtendedErrorCode,
            System.Int32 transactionReplyCode,
            System.UInt32 transactionID,
            System.Double orderNumber,
            System.String transactionReplyMessage);

        // -----------------------------------------------------------------
        // long TRANS2QUIK_API __stdcall TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK (
        // TRANS2QUIK_TRANSACTION_REPLY_CALLBACK pfTransactionReplyCallback, 
        // long* pnExtendedErrorCode, 
        // LPSTR lpstrErrorMessage, 
        // DWORD dwErrorMessageSize);
        // -----------------------------------------------------------------
        /// <summary>
        /// Функция устанавливает функцию обратного вызова TransactionReplyCallBack
        /// </summary>
        /// <param name="transactionReplyCallBack">Функция, которая будет обрабатывать информацию об отправленной транзакции</param>
        /// <param name="extendedErrorCode">В случае возникновения ошибки может содержать расширенный код ошибки</param>
        /// <param name="errorMessage">В случае возникновения ошибки может получать сообщение о возникшей ошибке</param>
        /// <param name="errorMessageSize">Содержит длину строки, на которую ссылается указатель errorMessage</param>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term>TRANS2QUIK_SUCCESS</term>
        /// <description>функция обратного вызова установлена</description>
        /// </item>
        /// <item>
        /// <term>TRANS2QUIK_FAILED</term>
        /// <description>функцию обратного вызова установить не удалось. В этом случае в переменную
        /// extendedErrorCode может передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        [DllImport("TRANS2QUIK.DLL", EntryPoint = "_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16", CallingConvention = CallingConvention.StdCall)]
        public static extern System.Int32 SET_TRANSACTIONS_REPLY_CALLBACK(
            TRANSACTION_REPLY_CALLBACK transactionReplyCallBack,
            System.Int32 extendedErrorCode,
            System.String errorMessage,
            System.UInt32 errorMessageSize);
        #endregion

        #region Public Method

        /// <summary>
        /// Устанавливает связь между библиотекой TRANS2QUIK.DLL с терминалом QUIK
        /// </summary>
        /// <param name="thePathToQUIK">Полный путь к терминалу QUIK</param>
        /// <returns>Возвращает значение типа <b>eQUIKResult</b> характеризующее результат. Может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>SUCCESS</term>
        /// <description>соединение установлено успешно</description>
        /// </item>
        /// <item>
        /// <term>QUIK_TERMINAL_NOT_FOUND</term>
        /// <description>в указанном каталоге либо отсутствует INFO.EXE, либо у него не запущен сервис обработки
        /// внешних подключений, в extendedErrorCode в этом случае передается 0</description>
        /// </item>
        /// <item>
        /// <term>DLL_VERSION_NOT_SUPPORTED</term>
        /// <description>используемая версия Trans2QUIK.DLL указанным INFO.EXE не поддерживается,
        /// в extendedErrorCode в этом случае передается 0</description>
        /// </item>
        /// <item>
        /// <term>ALREADY_CONNECTED_TO_QUIK</term>
        /// <description>соединение уже установлено, в extendedErrorCode в этом случае передается 0</description>
        /// </item>
        /// <item>
        /// <term>FAILED</term>
        /// <description>произошла ошибка при установлении соединения, в [EEC] в этом случае
        /// передается дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        public static eQUIKResult Connect(string thePathToQUIK)
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.CONNECT";
            result = (eQUIKResult)CONNECT(thePathToQUIK, ref m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Разрывает связь между библиотекой TRANS2QUIK.DLL с терминалом QUIK
        /// </summary>
        /// <returns>Возвращает значение типа <b>eQUIKResult</b> характеризующее результат. Может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>SUCCESS</term>
        /// <description>соединение библиотеки TRANS2QUIK.DLL с терминалом QUIK разорвано успешно</description>
        /// </item>
        /// <item>
        /// <term>DLL_NOT_CONNECTED</term>
        /// <description>попытка разорвать соединение при не установленной связи. В этом случае в
        /// [EEC] может передаваться дополнительный код ошибки</description>
        /// </item>
        /// <item>
        /// <term>FAILED</term>
        /// <description>произошла ошибка при разрыве соединения. В этом случае в
        /// [EEC] передается дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        /// 
        public static eQUIKResult Disconnect()
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.DISCONNECT";
            result = (eQUIKResult)DISCONNECT(ref m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Проверят соединение библиотеки TRANS2QUIK.DLL с терминалом QUIK
        /// </summary>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term><see cref="TRANS2QUIKSHARP.eQUIKResult"/>.DLL_CONNECTED</term>
        /// <description>соединение библиотеки TRANS2QUIK.DLL с терминалом QUIK установлено</description>
        /// </item>
        /// <item>
        /// <term><see cref="TRANS2QUIKSHARP.eQUIKResult"/>.DLL_NOT_CONNECTED</term>
        /// <description>соединение библиотеки TRANS2QUIK.DLL с терминалом QUIK не установлено</description>
        /// </item>
        /// </list>
        /// </returns>
        public static eQUIKResult TestConnectionDLL()
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.IS_DLL_CONNECTED";
            result = (eQUIKResult)IS_DLL_CONNECTED(ref m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Проверяет соединение терминала QUIK с сервером QUIK
        /// </summary>
        /// <returns>Возвращаемое число может принимать следующие значения:
        /// <list type ="table">
        /// <item>
        /// <term><see cref="TRANS2QUIKSHARP.eQUIKResult"/>.QUIK_CONNECTED</term>
        /// <description>соединение терминала QUIK с сервером QUIK установлено</description>
        /// </item>
        /// <item>
        /// <term><see cref="TRANS2QUIKSHARP.eQUIKResult"/>.QUIK_NOT_CONNECTED</term>
        /// <description>соединение терминала QUIK с сервером QUIK не установлено</description>
        /// </item>
        /// <item>
        /// <term><see cref="TRANS2QUIKSHARP.eQUIKResult"/>.DLL_NOT_CONNECTED</term>
        /// <description>не установлена связь библиотеки TRANS2QUIK.DLL с терминалом QUIK. В этом случае
        /// проверить наличие или отсутствие связи терминала QUIK с сервером невозможно </description>
        /// </item>
        /// </list>
        /// </returns>
        public static eQUIKResult TestConnectionQUIK()
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.IS_QUIK_CONNECTED";
            result = (eQUIKResult)IS_QUIK_CONNECTED(ref m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Отправляет транзакции в синхронном режиме
        /// </summary>
        /// <param name="theTransactionDescription">Строка с описанием транзакции</param>
        /// <param name="theOrderNumber">Если результат выполнения функции <b>SUCCESS</b>содержит номер заявки в торговой системе</param>
        /// <returns>Возвращает значение типа <b>eQUIKResult</b> характеризующее результат. Может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>SUCCESS</term>
        /// <description>транзакция отправлена успешно</description>
        /// </item>
        /// <item>
        /// <term>WRONG_SYNTAX</term>
        /// <description>строка транзакции заполнена неверно</description>
        /// </item>
        /// <item>
        /// <term>DLL_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между библиотекой TRANS2QUIK.DLL и терминалом QUIK</description>
        /// </item>
        /// <item>
        /// <term>QUIK_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между терминалом QUIK и сервером</description>
        /// </item>
        /// <item>
        /// <term>FAILED</term>
        /// <description>транзакцию отправить не удалось. В переменную extendedErrorCode в этом случае может
        /// передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        public static eQUIKResult SendSyncTransaction(string theTransactionDescription, double theOrderNumber)
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.SEND_SYNC_TRANSACTION";
            result = (eQUIKResult)SEND_SYNC_TRANSACTION(theTransactionDescription, ref m_RPC, ref m_TID,
                ref theOrderNumber, m_RSM, m_RMS, ref m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Отправляет транзакции в асинхронном режиме
        /// </summary>
        /// <param name="theTransactionDescription">Строка с описанием транзакции</param>
        /// 
        /// <returns>Возвращает значение типа <b>eQUIKResult</b> характеризующее результат. Может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>SUCCESS</term>
        /// <description>транзакция отправлена успешно</description>
        /// </item>
        /// <item>
        /// <term>WRONG_SYNTAX</term>
        /// <description>строка транзакции заполнена неверно</description>
        /// </item>
        /// <item>
        /// <term>DLL_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между библиотекой TRANS2QUIK.DLL и терминалом QUIK</description>
        /// </item>
        /// <item>
        /// <term>QUIK_NOT_CONNECTED</term>
        /// <description>отсутствует соединение между терминалом квик и сервером</description>
        /// </item>
        /// <item>
        /// <term>FAILED</term>
        /// <description>транзакцию отправить не удалось. В переменную extendedErrorCode в этом случае может
        /// передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        public static eQUIKResult SendAsyncTransaction(string theTransactionDescription)
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.SEND_ASYNC_TRANSACTION";
            result = (eQUIKResult)SEND_ASYNC_TRANSACTION(theTransactionDescription, ref m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Устанавливает функцию обратного вызова ConnectionStatusCallBack
        /// </summary>
        /// <param name="theConnectionStatusCallBack">Функция, которая будет обрабатывать информацию
        /// о состоянии связи библиотеки TRANS2QUIK.DLL с терминалом QUIK или терминала QUIK с сервером</param>
        /// <returns>Возвращает значение типа <b>eQUIKResult</b> характеризующее результат. Может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>SUCCESS</term>
        /// <description>функция обратного вызова установлена</description>
        /// </item>
        /// <item>
        /// <term>FAILED</term>
        /// <description>функцию обратного вызова установить не удалось. В этом случае в переменную 
        /// extendedErrorCode может передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        public static eQUIKResult SetConnectionStatusDelegate(CONNECTION_STATUS_CALLBACK theConnectionStatusCallBack)
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.SET_CONNECTION_STATUS_CALLBACK";
            result = (eQUIKResult)SET_CONNECTION_STATUS_CALLBACK(theConnectionStatusCallBack, m_EEC, m_ERM, m_EMS);
            //
            return result;
        }

        /// <summary>
        /// Устанавливает функцию обратного вызова TransactionReplyCallBack
        /// </summary>
        /// <param name="theTransactionReplyCallBack">Функция, которая будет обрабатывать информацию об отправленной транзакции</param>
        /// <returns>Возвращает значение типа <b>eQUIKResult</b> характеризующее результат. Может принимать следующие значения: 
        /// <list type ="table">
        /// <item>
        /// <term>SUCCESS</term>
        /// <description>функция обратного вызова установлена</description>
        /// </item>
        /// <item>
        /// <term>FAILED</term>
        /// <description>функцию обратного вызова установить не удалось. В этом случае в переменную
        /// extendedErrorCode может передаваться дополнительный код ошибки</description>
        /// </item>
        /// </list>
        /// </returns>
        public static eQUIKResult SetTransactionReplyDelegate(TRANSACTION_REPLY_CALLBACK theTransactionReplyCallBack)
        {
            // Locals
            eQUIKResult result = eQUIKResult.UNKNOWN;
            //
            m_LEF = "TRANS2QUIKSHARP.QUIKAPI.SET_TRANSACTIONS_REPLY_CALLBACK";
            result = (eQUIKResult)SET_TRANSACTIONS_REPLY_CALLBACK(theTransactionReplyCallBack, m_EEC, m_ERM, m_EMS);
            //
            return result;
        }
        #endregion
    }
}
