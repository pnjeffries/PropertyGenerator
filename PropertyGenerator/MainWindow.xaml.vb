Imports System.Text

Class MainWindow

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

    End Sub

    Public Sub UpdatePreview()
        If PreviewTB IsNot Nothing Then
            'PreviewTB.Text = GenerateOutput()
            Dim tr As New TextRange(PreviewTB.Document.ContentStart, PreviewTB.Document.ContentEnd)
            tr.Text = GenerateOutput()
        End If
    End Sub

    Public Function GenerateOutput() As String

        Dim propertyName As String = NameTB.Text
        Dim propertyType As String = TypeTB.Text
        Dim defaultValue As String = ValueTB.Text
        Dim description As String = DescriptionTB.Text
        Dim accessModifier As String = "public"
        If AccessCB.SelectedItem IsNot Nothing AndAlso TypeOf AccessCB.SelectedItem Is ComboBoxItem Then
            Dim cbi As ComboBoxItem = CType(AccessCB.SelectedItem, ComboBoxItem)
            accessModifier = cbi.Tag
        End If
        Dim language As String = "Java"
        If LanguageCB.SelectedItem IsNot Nothing AndAlso TypeOf LanguageCB.SelectedItem Is ComboBoxItem Then
            Dim cbi As ComboBoxItem = CType(LanguageCB.SelectedItem, ComboBoxItem)
            language = cbi.Tag
        End If

        Dim sb As New StringBuilder()

        If language = "Java" Then

            If Not String.IsNullOrEmpty(description) Then
                Dim capDescription As String = description.Substring(0, 1).ToUpper & description.Substring(1)
                sb.AppendLine("/**")

                sb.Append("* ")
                sb.AppendLine(capDescription)

                sb.AppendLine("*/")
            End If

            sb.Append("private ")
            If StaticCB.IsChecked Then
                sb.Append("static ")
            End If
            If TransientCB.IsChecked Then
                sb.Append("transient ")
            End If
            sb.Append(propertyType)
            sb.Append(" _")
            sb.Append(propertyName)
            If Not String.IsNullOrEmpty(defaultValue) AndAlso Not (LazyCB.IsChecked AndAlso GetterCB.IsChecked) Then
                sb.Append(" = ")
                sb.Append(defaultValue)
            End If
            sb.Append(";")
            sb.AppendLine()

            sb.AppendLine()

            'Getter:
            If GetterCB.IsChecked Then

                If Not String.IsNullOrEmpty(description) Then
                    'Javadoc
                    sb.AppendLine("/**")

                    sb.Append("* Get ")
                    sb.AppendLine(description)

                    sb.AppendLine("*/")
                End If

                sb.Append(accessModifier)
                sb.Append(" ")
                If StaticCB.IsChecked Then
                    sb.Append("static ")
                End If
                sb.Append(propertyType)
                sb.Append(" ")
                sb.Append(propertyName)
                sb.AppendLine("()")

                sb.AppendLine("{")

                If Not String.IsNullOrEmpty(defaultValue) AndAlso LazyCB.IsChecked Then
                    sb.Append(vbTab)
                    sb.Append("if (_")
                    sb.Append(propertyName)
                    sb.Append(" == null) _")
                    sb.Append(propertyName)
                    sb.Append(" = ")
                    sb.Append(defaultValue)
                    sb.AppendLine(";")
                End If

                sb.Append(vbTab)
                sb.Append("return _")
                sb.Append(propertyName)
                sb.AppendLine(";")

                sb.AppendLine("}")

                sb.AppendLine()

            End If

            'Setter:
            If SetterCB.IsChecked Then

                If Not String.IsNullOrEmpty(description) Then
                    'Javadoc
                    sb.AppendLine("/**")

                    sb.Append("* Set ")
                    sb.AppendLine(description)

                    sb.AppendLine("*/")
                End If

                sb.Append(accessModifier)
                If StaticCB.IsChecked Then
                    sb.Append(" static")
                End If
                sb.Append(" void ")
                sb.Append(propertyName)
                sb.Append("(")
                sb.Append(propertyType)
                sb.AppendLine(" value)")

                sb.AppendLine("{")

                sb.Append(vbTab)
                sb.Append("_")
                sb.Append(propertyName)
                sb.AppendLine(" = value;")

                If NotifyCB.IsChecked Then
                    sb.Append(vbTab)
                    sb.Append("notifyPropertyChanged(""")
                    sb.Append(propertyName)
                    sb.AppendLine(""");")
                End If

                sb.AppendLine("}")

                sb.AppendLine()

            End If

        ElseIf language = "VB" Then

            'VISUAL BASIC .NET:

            'Transient:
            If TransientCB.IsChecked Then
                sb.Append("<NonSerialized()>")
                sb.AppendLine()
            End If
            'Internal private member
            sb.Append("Private")
            If StaticCB.IsChecked Then
                sb.Append(" Shared")
            End If
            sb.Append(" _")
            sb.Append(propertyName)
            sb.Append(" As ")
            sb.Append(propertyType)
            If Not String.IsNullOrEmpty(defaultValue) AndAlso Not (LazyCB.IsChecked AndAlso GetterCB.IsChecked) Then
                sb.Append(" = ")
                sb.Append(defaultValue)
            End If
            sb.AppendLine()

            sb.AppendLine()

            'Comments:
            If Not String.IsNullOrEmpty(description) Then
                Dim capDescription As String = description.Substring(0, 1).ToUpper & description.Substring(1)
                sb.AppendLine("''' <summary>")

                sb.Append("''' ")
                sb.AppendLine(capDescription)

                sb.AppendLine("''' </summary>")
            End If

            'Property declaration:
            Dim capAccessModifier As String = accessModifier.Substring(0, 1).ToUpper & accessModifier.Substring(1)
            sb.Append(capAccessModifier)
            sb.Append(" ")
            If GetterCB.IsChecked AndAlso Not SetterCB.IsChecked Then
                sb.Append("ReadOnly ")
            ElseIf Not GetterCB.IsChecked AndAlso SetterCB.IsChecked Then
                sb.Append("WriteOnly ")
            End If
            If StaticCB.IsChecked Then
                sb.Append("Shared ")
            End If
            sb.Append("Property ")
            sb.Append(propertyName)
            sb.Append("() As ")
            sb.Append(propertyType)

            sb.AppendLine()

            If GetterCB.IsChecked Then
                sb.Append(vbTab)
                sb.Append("Get")
                sb.AppendLine()

                sb.Append(vbTab)
                sb.Append(vbTab)
                sb.Append("Return _")
                sb.Append(propertyName)
                sb.AppendLine()

                sb.Append(vbTab)
                sb.Append("End Get")
                sb.AppendLine()
            End If

            If SetterCB.IsChecked Then
                sb.Append(vbTab)
                sb.Append("Set(ByVal value As ")
                sb.Append(propertyType)
                sb.Append(")")
                sb.AppendLine()

                sb.Append(vbTab)
                sb.Append(vbTab)
                sb.Append("_")
                sb.Append(propertyName)
                sb.Append(" = value")
                sb.AppendLine()

                sb.Append(vbTab)
                sb.Append("End Set")
                sb.AppendLine()
            End If

            If GetterCB.IsChecked OrElse SetterCB.IsChecked Then
                sb.Append("End Property")
                sb.AppendLine()
            End If

        ElseIf language = "C#"

            'C#:

            'Internal private member
            sb.AppendLine("/// <summary>")
            sb.Append("/// Private backing member variable for the ")
            sb.Append(propertyName)
            sb.Append(" property")
            sb.AppendLine()
            sb.AppendLine("/// </summary>")

            'Transient:
            If TransientCB.IsChecked Then
                sb.Append("[NonSerialized()]")
                sb.AppendLine()
            End If

            sb.Append("private ")
            If StaticCB.IsChecked Then
                sb.Append("static ")
            End If
            sb.Append(propertyType)
            sb.Append(" _")
            sb.Append(propertyName)
            If Not String.IsNullOrEmpty(defaultValue) AndAlso Not (LazyCB.IsChecked AndAlso GetterCB.IsChecked) Then
                sb.Append(" = ")
                sb.Append(defaultValue)
            End If
            sb.Append(";")
            sb.AppendLine()

            sb.AppendLine()

            'Comment
            If Not String.IsNullOrEmpty(description) Then
                Dim capDescription As String = description.Substring(0, 1).ToUpper & description.Substring(1)
                sb.AppendLine("/// <summary>")

                sb.Append("/// ")
                sb.AppendLine(capDescription)

                sb.AppendLine("/// </summary>")
            End If

            'Property declaration
            sb.Append(accessModifier).Append(" ")
            If StaticCB.IsChecked Then
                sb.Append("static ")
            End If
            sb.Append(propertyType).Append(" ")
            sb.Append(propertyName).AppendLine()
            sb.AppendLine("{")

            If GetterCB.IsChecked Then
                sb.Append(vbTab)
                sb.Append("get")
                If LazyCB.IsChecked AndAlso Not String.IsNullOrEmpty(defaultValue) Then
                    sb.AppendLine().Append(vbTab).AppendLine("{")
                    sb.Append(vbTab).Append(vbTab).Append("if (_").Append(propertyName).Append(" == null ) _")
                    sb.Append(propertyName).Append(" = ").Append(defaultValue).Append(";").AppendLine()
                    sb.Append(vbTab).Append(vbTab).Append("return _").Append(propertyName).AppendLine(";")
                    sb.Append(vbTab).Append("}")
                    'TODO
                Else
                    sb.Append(" { return _").Append(propertyName).Append("; }")
                End If
                sb.AppendLine()
            End If

            If SetterCB.IsChecked Then
                sb.Append(vbTab).Append("set")
                If NotifyCB.IsChecked Then
                    sb.AppendLine().Append(vbTab).AppendLine("{")
                    sb.Append(vbTab).Append(vbTab)
                Else
                    sb.Append(" { ")
                End If
                sb.Append("_").Append(propertyName).Append(" = value;")
                If NotifyCB.IsChecked Then
                    sb.AppendLine.Append(vbTab).Append(vbTab)
                    sb.Append("NotifyPropertyChanged(""").Append(propertyName).Append(""");")
                    sb.AppendLine().Append(vbTab).Append("}")
                Else
                    sb.Append(" }")
                End If
            End If
            sb.AppendLine.AppendLine("}")
        End If

        Return sb.ToString

    End Function

    Private Sub CopyButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        Clipboard.SetText(GenerateOutput())
    End Sub

    Private Sub CloseButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        Me.Close()
    End Sub

    Private Sub ClearButton_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        NameTB.Text = ""
        TypeTB.Text = "double"
        ValueTB.Text = ""
        DescriptionTB.Text = ""
        GetterCB.IsChecked = True
        SetterCB.IsChecked = True
        TransientCB.IsChecked = False
        LazyCB.IsChecked = False
        NotifyCB.IsChecked = False
        StaticCB.IsChecked = False
    End Sub

    Private Sub CopyToDefault_Click(sender As Object, e As RoutedEventArgs)
        Dim str As String = TypeTB.Text
        If str IsNot Nothing AndAlso str.Length > 0 Then

            ValueTB.Text = "new " & str & "()"
        End If
    End Sub

End Class
