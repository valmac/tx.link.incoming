//http://quik.ru/user/forum/import/24842/24842/
//Re: API. С чем его едят?
//Это относится только к примеру на Visual Basic. Пример на Visual С++ я не смотрел. 
//Пример написан на старом языке VBA и в новые версии, такие как VB2005 или 
//VB2008 переносится с изменениями. Лучше всего это сделать автоматически. 
//Для этого можно создать новый пустой проект в старом VB6 и положить 
//туда модуль с примером. Затем открываем этот проект в новом VB, 
//и ПОЧТИ ВСЕ конвертируется как положено:
//1. Например везде в описаниях функций Long заменится на Integer.
//2. Изменится описание lpstrErrorMessage:
//вместо
//Dim lpstrErrorMessage As String * 250
//будет
//Dim lpstrErrorMessage As New VB6.FixedLengthString(250)
//в выводе сообщения пишем
//lpstrErrorMessage.Value
//Остальное нужно делать руками:
//1. вместо самописной функции LPCSTRtoString лучше добавить в начало программы
//Imports System.Runtime.InteropServices.Marshal
//и выводить сообщение так:
//ErrorMessage = PtrToStringAnsi(CType(lpstrErrorMessage, IntPtr))
//2. Ну и самая большая засада — AddressOf для Callback фунций. 
//Нужно создать делегатов и обязательно определить указатели в начале программы, 
//иначе они будут уничтожены сборщиком мусора.
//-
//Создаем делегатов для прототипов callback функций с тем же набором параметров:
//-
//Public Delegate Function TRANS2QUIK_CONNECTION_STATUS_CALLBACK( _
//ByVal nConnectionEvent As Integer, _
//ByVal nExtendedErrorCode As Integer, ByVal lpstrErrorMessage As Integer) As Integer
//Public Delegate Function TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK( _
//ByVal nTransactionResult As Integer, _
//ByVal nTransactionExtendedErrorCode As Integer, _
//ByVal nTransactionReplyCode As Integer, ByVal dwTransId As Integer, ByVal dOrderNum As Double, _
//ByVal lpstrTransactionReplyMessage As Integer) As Integer
//-
//Изменяем описание ..SET.. функций, включив туда этих делегатов как тип:
//-
//Public Declare Function TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK Lib «Trans2Quik.dll» 
//    Alias «_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16» ( _
//ByVal pfConnectionStatusCallback As TRANS2QUIK_CONNECTION_STATUS_CALLBACK, _
//ByRef pnExtendedErrorCode As Integer, ByVal lpstrErrorMessage As String, 
//    ByVal dwErrorMessageSize As Integer) As Integer
//Public Declare Function TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK Lib «Trans2Quik.dll» 
//    Alias «_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16» ( _
//ByVal pfTransactionReplyCallback As TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK, _
//ByRef pnExtendedErrorCode As Integer, ByVal lpstrErrorMessage As String, 
//    ByVal dwErrorMessageSize As Integer) As Integer
//-
//Изменяем название прототипов callback функций, добавив например в конце_REPORT
//-
//Public Shared Function TRANS2QUIK_CONNECTION_STATUS_CALLBACK_REPORT( _
//ByVal nConnectionEvent As Integer, _
//ByVal nExtendedErrorCode As Integer, ByVal lpstrErrorMessage As Integer) As Integer
//Public Shared Function TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK_REPORT( _
//ByVal nTransactionResult As Integer, _
//ByVal nTransactionExtendedErrorCode As Integer, ByVal nTransactionReplyCode As Integer, _
//ByVal dwTransId As Integer, ByVal dOrderNum As Double, 
//    ByVal lpstrTransactionReplyMessage As Integer) As Integer
//-
//определяем указатели на прототипы функций:
//-
//Public pfConnectionStatusCallback As TRANS2QUIK_CONNECTION_STATUS_CALLBACK = 
//    AddressOf TRANS2QUIK_CONNECTION_STATUS_CALLBACK_REPORT
//Public pfTransactionReplyCallback As TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK = 
//    AddressOf TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK_REPORT
//-
//и наконец при вызове ..SET.. функций пишем эти указатели:
//-
//TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK(
//    pfConnectionStatusCallback, pnExtendedErrorCode, lpstrErrorMessage.Value, dwErrorMessageSize)
//TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK(
//    pfTransactionReplyCallback, pnExtendedErrorCode, lpstrErrorMessage.Value, dwErrorMessageSize)
//-
//Теперь все работает.