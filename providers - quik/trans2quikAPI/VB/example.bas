
Public Sub Example()
    Dim FunctionResult As Long
    Dim pnExtendedErrorCode As Long
    Dim lpstrErrorMessage As String * 250
    Const dwErrorMessageSize = 250
    Dim nReturnCode As Long
    Dim dwTransId As Long
    Dim dOrderNum As Double
    Dim lpstrResultMessage As String * 250
    Const dwResultMessageSize = 250
    Dim TransStr As String

    FunctionResult = TRANS2QUIK_CONNECT("D:\Programm\KRIS5_100001\", pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)

    FunctionResult = TRANS2QUIK_IS_DLL_CONNECTED(pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)

    FunctionResult = TRANS2QUIK_IS_QUIK_CONNECTED(pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)

    TransStr = "ACTION=NEW_ORDER; TRANS_ID=208; CLASSCODE=EQBR; SECCODE=EESR; ACCOUNT=L01-00000F00; CLIENT_CODE=Q7;" & _
    "TYPE=L; OPERATION=B; QUANTITY=1; PRICE=26;"

    FunctionResult = TRANS2QUIK_SEND_SYNC_TRANSACTION(TransStr, nReturnCode, dwTransId, dOrderNum, lpstrResultMessage, _
    dwResultMessageSize, pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)

    FunctionResult = TRANS2QUIK_DISCONNECT(pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)

End Sub


Function TRANS2QUIK_CONNECTION_STATUS_CALLBACK(ByVal nConnectionEvent As Long, ByVal nExtendedErrorCode As Long, ByVal lpstrErrorMessage As Long) As Long
On Error Resume Next
    Dim ErrorMessage As String
    Dim nConnectionEventString As String
    ErrorMessage = LPCSTRtoString(lpstrErrorMessage)
    nConnectionEventString = Trans2QuikResultToStr(nConnectionEvent)
    MsgBox "CB:" & vbCrLf & _
        "nConnectionEvent: " & nConnectionEvent & vbCrLf & _
        "nConnectionEventString: " & nConnectionEventString & vbCrLf & _
        "nExtendedErrorCode : " & nExtendedErrorCode
    ConnectionCallback = TRANS2QUIK_SUCCESS
End Function


Function TRANS2QUIK_TRANSACTIONS_REPLY_CALLBACK(ByVal nTransactionResult As Long, ByVal nTransactionExtendedErrorCode As Long, ByVal nTransactionReplyCode As Long, ByVal dwTransId As Long, ByVal dOrderNum As Double, ByVal lpstrTransactionReplyMessage As Long) As Long
On Error Resume Next
    Dim ResultMessage As String
    Dim nTransactionResultString As String
    ResultMessage = LPCSTRtoString(lpstrTransactionReplyMessage)
    nTransactionResultString = Trans2QuikResultToStr(nTransactionResult)
    MsgBox "CB:" & vbCrLf & _
        "nTransactionResult: " & nTransactionResult & vbCrLf & _
        "nTransactionResultString: " & nTransactionResultString & vbCrLf & _
        "nTransactionExtendedErrorCode : " & nTransactionExtendedErrorCode & vbCrLf & _
        "TransactionReplyCode : " & nTransactionReplyCode & vbCrLf & _
        "Trans id: " & dwTransId & vbCrLf & _
        "OrderNum: " & dOrderNum & vbCrLf & _
        "ResultMessage: " & ResultMessage
    TransCallback = TRANS2QUIK_SUCCESS
End Function