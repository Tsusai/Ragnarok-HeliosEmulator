object MainForm: TMainForm
  Left = 382
  Top = 184
  Width = 469
  Height = 196
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Helios'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    461
    146)
  PixelsPerInch = 96
  TextHeight = 13
  object Console: TMemo
    Left = 0
    Top = 0
    Width = 466
    Height = 113
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WantTabs = True
  end
  object ConsoleIn: TEdit
    Left = 0
    Top = 108
    Width = 466
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    OnKeyPress = ConsoleInKeyPress
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 129
    Width = 461
    Height = 17
    Panels = <
      item
        Text = 'Accounts : 0'
        Width = 50
      end>
  end
  object LoginServer: TIdTCPServer
    Active = True
    Bindings = <>
    CommandHandlers = <>
    CommandHandlersEnabled = False
    DefaultPort = 6900
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnConnect = LoginServerConnect
    OnExecute = LoginServerExecute
    OnException = ServerException
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 432
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 400
    Top = 8
    object FileMenu: TMenuItem
      Caption = 'File'
      object ExitButton: TMenuItem
        Caption = 'Exit'
        OnClick = ExitButtonClick
      end
    end
  end
  object CharaServer: TIdTCPServer
    Active = True
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 6121
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnExecute = CharaServerExecute
    OnException = ServerException
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 432
    Top = 40
  end
  object CharaToLogin: TIdTCPClient
    MaxLineAction = maException
    Port = 0
    Left = 400
    Top = 40
  end
  object ZoneServer: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 5121
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 432
    Top = 72
  end
  object ZoneToChara: TIdTCPClient
    MaxLineAction = maException
    Port = 0
    Left = 400
    Top = 72
  end
end
