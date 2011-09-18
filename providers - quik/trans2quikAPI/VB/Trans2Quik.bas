
Public Declare Function TRANS2QUIK_SEND_SYNC_TRANSACTION Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_SEND_SYNC_TRANSACTION@36" _
    (ByVal lpstTransactionString As String, ByRef pnReplyCode As Long, ByRef pdwTransId As Long, ByRef pdOrderNum As Double, _
     ByVal lpstrResultMessage As String, ByVal dwResultMessageSize As Long, _
     ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_SEND_ASYNC_TRANSACTION Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_SEND_ASYNC_TRANSACTION@16" _
    (ByVal lpstTransactionString As String, _
     ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_CONNECT Lib "Trans2Quik.dll" _
    Alias "_TRANS2QUIK_CONNECT@16" _
    (ByVal lpstConnectionParamsString As String, _
     ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_DISCONNECT Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_DISCONNECT@12" _
     (ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16" _
     (ByVal pfConnectionStatusCallback As Long, _
     ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16" _
     (ByVal pfTransactionReplyCallback As Long, _
     ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_IS_QUIK_CONNECTED Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_IS_QUIK_CONNECTED@12" _
     (ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long

Public Declare Function TRANS2QUIK_IS_DLL_CONNECTED Lib "Trans2Quik.dll" _
     Alias "_TRANS2QUIK_IS_DLL_CONNECTED@12" _
     (ByRef pnExtendedErrorCode As Long, _
     ByVal lpstrErrorMessage As String, ByVal dwErrorMessageSize As Long) As Long


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
    FunctionResult = TRANS2QUIK_CONNECT("Programm\KRIS5_100001\", pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)
    FunctionResult = TRANS2QUIK_IS_DLL_CONNECTED(pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)
    FunctionResult = TRANS2QUIK_IS_QUIK_CONNECTED(pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)
    TransStr = "ACTION=NEW_ORDER; TRANS_ID=208; CLASSCODE=EQBR; SECCODE=EESR; ACCOUNT=L01-00000F00; CLIENT_CODE=Q7;" & _
    "TYPE=L; OPERATION=B; QUANTITY=1; PRICE=26;"
    FunctionResult = TRANS2QUIK_SEND_SYNC_TRANSACTION(TransStr, nReturnCode, dwTransId, dOrderNum, lpstrResultMessage, _
    dwResultMessageSize, pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)
    FunctionResult = TRANS2QUIK_DISCONNECT(pnExtendedErrorCode, lpstrErrorMessage, dwErrorMessageSize)
End Sub


Function LPCSTRtoString(ByVal lpstrStr As Long) As String
    LPCSTRtoString = ""
    Dim cChars As Long
    If lpstrStr = 0 Then
        Exit Function
    End If
    cChars = lstrlen(lpstrStr)
    If cChars = 0 Then
        Exit Function
    End If
    Dim TmpBuffer() As Byte
    ReDim TmpBuffer(1 To cChars + 1) As Byte
    CopyMemory TmpBuffer(1), ByVal lpstrStr, cChars + 1
    For i = 1 To cChars + 1
       LPCSTRtoString = LPCSTRtoString & Chr(TmpBuffer(i))
    Next
End Function
