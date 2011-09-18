object Form3: TForm3
  Left = 473
  Top = 469
  BorderStyle = bsDialog
  Caption = #1053#1086#1074#1072#1103' '#1090#1072#1073#1083#1080#1094#1072
  ClientHeight = 72
  ClientWidth = 169
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 22
    Height = 13
    Caption = #1048#1084#1103
  end
  object Edit1: TEdit
    Left = 48
    Top = 12
    Width = 113
    Height = 21
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
  object Button1: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 88
    Top = 40
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
  end
end
