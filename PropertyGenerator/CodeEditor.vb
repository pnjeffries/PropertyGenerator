Imports System.ComponentModel
Imports System.Timers
Imports System.IO
Imports System.Windows.Controls.Primitives

Public Class CodeEditor
    Inherits RichTextBox
    Implements INotifyPropertyChanged

    'Syntax
    Private _Syntax As New SyntaxData

    'Intellisense
    Public Property IntellisensePopup As Popup = Nothing
    Public Property IntellisenseWordList As ListBox = Nothing
    Public Property IntellisenseRange As TextRange = Nothing

    'Chunks
    Private _RootChunk As CodeChunk
    Private Property RootChunk As CodeChunk
        Get
            If _RootChunk Is Nothing Then
                _RootChunk = New CodeChunk(True)
            End If
            Return _RootChunk
        End Get
        Set(value As CodeChunk)
            _RootChunk = value
        End Set
    End Property

    Public ReadOnly Property Chunks As ExtendedObservableCollection(Of CodeChunk)
        Get
            Return RootChunk.SubChunks
        End Get
    End Property

    Public ReadOnly Property GlobalVariables As ExtendedObservableCollection(Of CodeChunk)
        Get
            Return RootChunk.Variables
        End Get
    End Property

    'IO
    Public Property FileName As String
    Private _ModifiedSinceLastSave As Boolean = False
    Public Property StartUpPath As String

    'Brushes
    Public Property NormalBrush As Brush = Brushes.Black
    Public Property KeywordBrush As Brush = Brushes.Blue
    Public Property StringLiteralBrush As Brush = Brushes.DarkRed
    Public Property CommentBrush As Brush = Brushes.Green
    Public Property NumericBrush As Brush = Brushes.Red

    'Indenting
    Public Property IndentSpacing As Double = 0

    'Processing queue:
    Private WithEvents _QueueTimer As Timer
    Private _QueueCount As Integer = 0
    Private _NextBlockIndex As Integer = 0
    Private _NextBlock As Block = Nothing
    Private _PriorityUpdates As List(Of Integer)

    Private _IgnoreUpdates As Boolean = False

    Public Sub New()
        _QueueTimer = New Timer(10)
        _QueueTimer.Start()
        _PriorityUpdates = New List(Of Integer)
    End Sub

    ''' <summary>
    ''' Notify any bound controls that a property of this object has been modified
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    ''' <remarks></remarks>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

    ''' <summary>
    ''' Let it be known that a bindable property of this object has been modified
    ''' </summary>
    ''' <param name="p1"></param>
    ''' <remarks></remarks>
    Private Sub NotifyPropertyChanged(ByVal p1 As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(p1))
    End Sub

    Public Sub InjectIntellisenseSelection()
        If IntellisensePopup IsNot Nothing AndAlso IntellisensePopup.IsOpen AndAlso IntellisenseWordList IsNot Nothing AndAlso IntellisenseWordList.SelectedItem IsNot Nothing Then
            If TypeOf IntellisenseWordList.SelectedItem Is CodeChunk Then
                Dim chunk As CodeChunk = CType(IntellisenseWordList.SelectedItem, CodeChunk)
                If IntellisenseRange IsNot Nothing Then
                    IntellisenseRange.Text = chunk.Name
                    CaretPosition = IntellisenseRange.End
                End If
                IntellisensePopup.IsOpen = False
                Focus()
            End If
        End If
    End Sub

    Private Sub CodeEditor_PreviewKeyDown(sender As Object, e As System.Windows.Input.KeyEventArgs) Handles Me.PreviewKeyDown
        If e.Key = Key.Down Then
            If IntellisensePopup IsNot Nothing AndAlso IntellisensePopup.IsOpen Then
                e.Handled = True
                If IntellisenseWordList IsNot Nothing Then
                    IntellisenseWordList.SelectedIndex += 1
                End If
            End If
        ElseIf e.Key = Key.Up Then
            If IntellisensePopup IsNot Nothing AndAlso IntellisensePopup.IsOpen Then
                e.Handled = True
                If IntellisenseWordList IsNot Nothing Then
                    IntellisenseWordList.SelectedIndex -= 1
                End If
            End If
        ElseIf e.Key = Key.Return Then
            If IntellisensePopup IsNot Nothing AndAlso IntellisensePopup.IsOpen Then
                e.Handled = True
                InjectIntellisenseSelection()
            End If
        End If

    End Sub

    Private Sub SyntaxTextBox_TextChanged(ByVal sender As Object, ByVal e As System.Windows.Controls.TextChangedEventArgs) Handles Me.TextChanged

        If Not _IgnoreUpdates Then
            If e.Changes.Count > 0 Then
                Dim firstChange As TextChange = e.Changes(0)
                If firstChange.AddedLength > 0 OrElse firstChange.RemovedLength > 0 Then
                    _ModifiedSinceLastSave = True
                    If Me.Document IsNot Nothing AndAlso CaretPosition IsNot Nothing AndAlso CaretPosition.Paragraph IsNot Nothing Then
                        _NextBlock = CaretPosition.Paragraph.PreviousBlock
                    End If
                End If
            End If
        End If

    End Sub

    Private Function GetChunk(ByVal name As String) As CodeChunk
        Return RootChunk.GetChunk(name)
    End Function

    Private Function GetVariable(ByVal name As String) As CodeChunk
        Return RootChunk.GetVariable(name)
    End Function

    Private Sub RemoveChunk(ByVal chunk As CodeChunk)

        If chunk.Parent Is Nothing Then
            'Get rid of chunk if it's top level:
            If Chunks.Contains(chunk) Then
                Chunks.Remove(chunk)
            End If
        Else
            If chunk.Parent.SubChunks.Contains(chunk) Then
                chunk.Parent.SubChunks.Remove(chunk)
            ElseIf chunk.Parent.SubChunks.Contains(chunk) Then
                chunk.Parent.Variables.Remove(chunk)
            End If
        End If

    End Sub

    Private Sub ProcessParagraph(ByRef paragraph As Paragraph)

        _IgnoreUpdates = True

        If paragraph IsNot Nothing Then

            AutoIndent(paragraph)

            Dim tag As CodeParagraphTag = GetParagraphTag(paragraph)
            Dim nextTag As CodeParagraphTag = Nothing
            If paragraph.NextBlock IsNot Nothing AndAlso TypeOf paragraph.NextBlock Is Paragraph Then
                nextTag = GetParagraphTag(CType(paragraph.NextBlock, Paragraph))
            End If
            Dim previousTag As CodeParagraphTag = Nothing
            If paragraph.PreviousBlock IsNot Nothing AndAlso TypeOf paragraph.PreviousBlock Is Paragraph Then
                previousTag = GetParagraphTag(CType(paragraph.PreviousBlock, Paragraph))
            End If

            'Propagate block comments:
            If nextTag IsNot Nothing Then
                nextTag.IsBlockComment = tag.IsBlockComment
            End If

            Dim nextRange As New TextRange(paragraph.ContentStart, paragraph.ContentEnd)
            Dim text As String = nextRange.Text

            If tag.IsBlockComment Then
                nextRange.ApplyPropertyValue(TextElement.ForegroundProperty, CommentBrush)
            Else
                nextRange.ApplyPropertyValue(TextElement.ForegroundProperty, NormalBrush)
            End If

            Dim isBeingEdited As Boolean = False
            Dim showIntellisense As Boolean = False
            If CaretPosition.Paragraph Is paragraph Then
                isBeingEdited = True
                'showIntellisense = True
            End If

            Dim intellisenseStart As TextPointer = Nothing

            Dim keyWordRanges As New List(Of TextRange)
            Dim stringLiteralRanges As New List(Of TextRange)
            Dim commentRanges As New List(Of TextRange)
            Dim numericRanges As New List(Of TextRange)

            Dim functionDeclaration As Boolean = False
            Dim methodDeclaration As Boolean = False
            Dim propertyDeclaration As Boolean = False
            Dim variableDeclaration As Boolean = False
            Dim chunkDeclared As CodeChunk = Nothing
            Dim targetChunk As CodeChunk = Nothing

            Dim isAssignment As Boolean = False

            Dim indentCount As Integer = 0
            Dim outdentCount As Integer = 0

            Dim state As ParseState = ParseState.Normal
            If tag.IsBlockComment Then
                state = ParseState.Comment
            End If

            Dim wordStart As Integer = 0
            Dim i As Integer = 0
            Dim lastC As Char
            While i < text.Length
                Dim c As Char = text(i)
                If state = ParseState.Normal Then
                    'Normal highlighting:
                    If Char.IsWhiteSpace(c) OrElse _Syntax.IsDelimiter(c) Then
                        Dim moveWordStart As Boolean = True
                        If showIntellisense Then 'Cancel showing intellisense if the caret is somewhere else
                            Dim rangeEnd As TextPointer = paragraph.ContentStart.GetPositionAtOffset(i + 1)
                            If CaretPosition.CompareTo(rangeEnd) > 0 Then
                                showIntellisense = False
                            End If
                        End If
                        If i > wordStart Then
                            Dim word As String = text.Substring(wordStart, i - wordStart)
                            If _Syntax.IsNumeric(word) Then 'Built in IsNumeric doesn't work for some reason...
                                If _Syntax.IsDecimalSeparator(c) Then
                                    moveWordStart = False 'Treat the decimal as part of the number
                                Else
                                    Dim rangeStart As TextPointer = paragraph.ContentStart.GetPositionAtOffset(wordStart + 1)
                                    Dim rangeEnd As TextPointer = paragraph.ContentStart.GetPositionAtOffset(i + 1)
                                    Dim range As New TextRange(rangeStart, rangeEnd)
                                    numericRanges.Add(range)
                                End If
                            ElseIf _Syntax.IsKeyWord(word) Then
                                Dim rangeStart As TextPointer = paragraph.ContentStart.GetPositionAtOffset(wordStart + 1)
                                Dim rangeEnd As TextPointer = paragraph.ContentStart.GetPositionAtOffset(i + 1)
                                Dim range As New TextRange(rangeStart, rangeEnd)
                                keyWordRanges.Add(range)

                                'Function analysis:
                                If Char.IsWhiteSpace(c) AndAlso _Syntax.IsFunctionKeyword(word) Then
                                    functionDeclaration = True
                                Else
                                    functionDeclaration = False
                                End If

                                'variable declaration analysis:
                                If Char.IsWhiteSpace(c) AndAlso _Syntax.IsVariableDeclarationKeyword(word) Then
                                    variableDeclaration = True
                                Else
                                    variableDeclaration = False
                                End If

                                'method analysis:
                                If _Syntax.IsAccessor(c) Then
                                    If _Syntax.IsPrototypeKeyword(word) Then
                                        methodDeclaration = True
                                        propertyDeclaration = False
                                    ElseIf _Syntax.IsSelfReference(word) Then
                                        propertyDeclaration = True
                                        methodDeclaration = False
                                        If isBeingEdited Then
                                            Dim nearCaret As Integer = CaretPosition.CompareTo(rangeEnd)
                                            If nearCaret > 0 Then
                                                'Houston, we have potential intellisense!
                                                showIntellisense = True
                                                intellisenseStart = rangeEnd.GetPositionAtOffset(1)
                                            End If
                                        End If
                                    Else
                                        methodDeclaration = False
                                        propertyDeclaration = False
                                    End If

                                End If

                                
                            Else
                                'Could this be a function or class definition?
                                If functionDeclaration Then
                                    Dim functionName As String = word
                                    Dim chunk As CodeChunk = GetChunk(functionName)
                                    chunkDeclared = chunk
                                    functionDeclaration = False
                                ElseIf methodDeclaration Then
                                    Dim splitWords() As String = text.Split(_Syntax.DefaultAccessor())
                                    If splitWords.Length > 0 Then
                                        Dim className As String = splitWords(0)
                                        If Chunks.ContainsStringDescriptor(className) Then
                                            Dim classChunk As CodeChunk = GetChunk(className)
                                            Dim functionName As String = word
                                            Dim chunk As CodeChunk = classChunk.GetChunk(functionName)
                                            chunk.IsProperty = False
                                            chunkDeclared = chunk
                                        End If
                                    End If
                                    methodDeclaration = False
                                ElseIf propertyDeclaration Then
                                    If tag.Context IsNot Nothing Then
                                        Dim classChunk As CodeChunk = tag.Context.GetTopLevelChunk()
                                        Dim propertyName As String = word
                                        Dim chunk As CodeChunk
                                        If Not classChunk.SubChunks.ContainsStringDescriptor(propertyName) Then
                                            chunk = classChunk.GetChunk(propertyName)
                                            chunk.IsProperty = True
                                            chunkDeclared = chunk
                                        ElseIf tag.DeclarationOf IsNot Nothing Then
                                            chunk = classChunk.GetChunk(propertyName)
                                            If tag.DeclarationOf Is chunk Then
                                                chunkDeclared = chunk
                                            End If
                                        End If
                                    End If
                                    propertyDeclaration = False
                                ElseIf variableDeclaration Then
                                    If tag.Context IsNot Nothing Then
                                        'local variable!
                                        Dim functionChunk As CodeChunk = tag.Context
                                        Dim propertyName As String = word
                                        Dim chunk As CodeChunk
                                        If Not functionChunk.Variables.ContainsStringDescriptor(propertyName) Then
                                            chunk = functionChunk.GetVariable(propertyName)
                                            chunk.IsProperty = True
                                            chunkDeclared = chunk
                                        ElseIf tag.DeclarationOf IsNot Nothing Then
                                            chunk = functionChunk.GetVariable(propertyName)
                                            If tag.DeclarationOf Is chunk Then
                                                chunkDeclared = chunk
                                            End If
                                        End If
                                    Else
                                        'Global variable! Coo!
                                        Dim propertyName As String = word
                                        Dim chunk As CodeChunk
                                        If Not GlobalVariables.ContainsStringDescriptor(propertyName) Then
                                            chunk = GetVariable(propertyName)
                                            chunk.IsProperty = True
                                            chunkDeclared = chunk
                                        ElseIf tag.DeclarationOf IsNot Nothing Then
                                            chunk = GetVariable(propertyName)
                                            If tag.DeclarationOf Is chunk Then
                                                chunkDeclared = chunk
                                            End If
                                        End If
                                    End If
                                    variableDeclaration = False
                                Else
                                    'Could be a variable reference...
                                    Dim variable As CodeChunk
                                    If tag.Context IsNot Nothing Then
                                        variable = tag.Context.IsInScopeVariable(word)
                                    Else
                                        variable = RootChunk.IsInScopeVariable(word)
                                    End If
                                    If variable IsNot Nothing Then 'Found a matching variable name in the current scope
                                        If isAssignment Then
                                            'could be being assigned to something else...
                                        Else
                                            'Could be the target of an assignment...
                                        End If
                                    End If
                                End If
                            End If
                        End If

                        'assignment analysis
                        If _Syntax.IsAssignmentOperator(c) Then
                            isAssignment = True
                        End If

                        If _Syntax.IsIndentDelimiter(c) Then
                            indentCount += 1
                        ElseIf _Syntax.IsOutdentDelimiter(c) Then
                            outdentCount += 1
                        End If
                        If moveWordStart Then
                            wordStart = i + 1
                        End If
                    End If
                    If _Syntax.IsStringLiteralDelimiter(c) Then
                        state = ParseState.StringLiteral
                        wordStart = i
                    End If
                    If i > 0 Then
                        Dim doubleSymbol As String = lastC & c
                        If _Syntax.IsInlineCommentDelimiter(doubleSymbol) OrElse _Syntax.IsInlineCommentDelimiter(c) Then
                            'Comment - highlight the rest of the line as a comment
                            state = ParseState.Comment
                            Dim rangeStart As TextPointer = paragraph.ContentStart.GetPositionAtOffset(i)
                            Dim rangeEnd As TextPointer = paragraph.ContentEnd
                            Dim range As New TextRange(rangeStart, rangeEnd)
                            commentRanges.Add(range)
                            i = text.Length
                            If chunkDeclared IsNot Nothing Then
                                Dim comment As String = range.Text.TrimStart("/")
                                If chunkDeclared.AttachedComment <> comment Then
                                    chunkDeclared.AttachedComment = comment
                                End If
                            End If
                            'Block comment starting - propagate onwards
                            If _Syntax.IsBlockCommentStartDelimiter(doubleSymbol) Then
                                If nextTag IsNot Nothing Then
                                    nextTag.IsBlockComment = True
                                End If
                            End If
                        End If
                    End If
                ElseIf state = ParseState.StringLiteral Then
                    'String highlighting - ignore everything until we come to another string delimiter
                    If _Syntax.IsStringLiteralDelimiter(c) OrElse i = text.Length - 1 Then
                        Dim rangeStart As TextPointer = paragraph.ContentStart.GetPositionAtOffset(wordStart + 1)
                        Dim rangeEnd As TextPointer = paragraph.ContentStart.GetPositionAtOffset(i + 2)
                        Dim range As New TextRange(rangeStart, rangeEnd)
                        stringLiteralRanges.Add(range)
                        state = ParseState.Normal
                        wordStart = i + 1
                    End If
                ElseIf state = ParseState.Comment Then
                    'if we're here we must be in a block comment - look for a way out!
                    If i > 0 Then
                        Dim doubleSymbol As String = lastC & c
                        If _Syntax.IsBlockCommentEndDelimiter(doubleSymbol) Then
                            'Comment - highlight the rest of the line as a comment
                            state = ParseState.Normal
                            If nextTag IsNot Nothing Then
                                nextTag.IsBlockComment = False
                            End If
                        End If
                    End If
                End If
                i += 1
                lastC = c
            End While


            'Tidy up declared chunks:
            If tag.DeclarationOf IsNot Nothing AndAlso Not tag.DeclarationOf Is chunkDeclared Then
                Dim chunk As CodeChunk = tag.DeclarationOf
                RemoveChunk(chunk)
            End If
            tag.DeclarationOf = chunkDeclared
            If chunkDeclared IsNot Nothing AndAlso chunkDeclared.DefinedAt Is Nothing Then
                chunkDeclared.DefinedAt = paragraph
            End If

            If chunkDeclared IsNot Nothing AndAlso Not chunkDeclared.IsProperty Then
                tag.Context = chunkDeclared
                If nextTag IsNot Nothing Then
                    nextTag.Context = tag.Context
                End If
            Else
                If tag.IndentLevels > 0 AndAlso previousTag IsNot Nothing Then
                    tag.Context = previousTag.Context
                End If
            End If

            'Propagate indent and context:
            If nextTag IsNot Nothing Then
                nextTag.IndentLevels = tag.IndentLevels + indentCount - outdentCount
                If nextTag.IndentLevels > 0 Then
                    nextTag.Context = tag.Context
                End If
            End If

            'Apply highlighting:
            For Each range As TextRange In keyWordRanges
                range.ApplyPropertyValue(TextElement.ForegroundProperty, KeywordBrush)
            Next
            For Each range As TextRange In stringLiteralRanges
                range.ApplyPropertyValue(TextElement.ForegroundProperty, StringLiteralBrush)
            Next
            For Each range As TextRange In commentRanges
                range.ApplyPropertyValue(TextElement.ForegroundProperty, CommentBrush)
            Next
            For Each range As TextRange In numericRanges
                range.ApplyPropertyValue(TextElement.ForegroundProperty, NumericBrush)
            Next

            tag.PreviousText = text

            'Intellisense:
            If showIntellisense Then
                If intellisenseStart IsNot Nothing Then
                    Dim range As New TextRange(intellisenseStart, CaretPosition)
                    Dim incompleteText As String = range.Text
                    If tag.Context IsNot Nothing Then
                        Dim parentChunk As CodeChunk = tag.Context.GetTopLevelChunk
                        If IntellisensePopup IsNot Nothing Then
                            Dim positionRect As Rect = CaretPosition.GetCharacterRect(LogicalDirection.Forward)
                            IntellisensePopup.PlacementTarget = Me
                            IntellisensePopup.PlacementRectangle = positionRect
                            IntellisensePopup.Tag = paragraph
                            IntellisenseWordList.ItemsSource = parentChunk.SubChunks
                            IntellisensePopup.IsOpen = True
                            If incompleteText.Length > 0 AndAlso tag.PreviousText <> text Then
                                Dim suggestion As CodeChunk = parentChunk.GetSubChunkStartMatch(incompleteText)
                                If suggestion IsNot Nothing Then
                                    IntellisenseWordList.SelectedItem = suggestion
                                End If
                            End If
                            IntellisenseRange = range
                        End If
                    End If
                End If
            Else
                If IntellisensePopup IsNot Nothing AndAlso IntellisensePopup.Tag Is paragraph Then
                    IntellisensePopup.IsOpen = False
                End If
            End If

        End If

        _IgnoreUpdates = False

    End Sub

    Private Delegate Sub ProcessNextBlockDelegate()

    Private Sub ProcessNextBlock()
        Dim block As Block = Nothing
        If Document IsNot Nothing AndAlso Document.Blocks IsNot Nothing Then
            'If _NextBlockIndex >= Document.Blocks.Count Then
            '    _NextBlockIndex = 0
            'End If
            'If _NextBlockIndex < Document.Blocks.Count Then
            '    block = Document.Blocks(_NextBlockIndex)
            'End If
            If _NextBlock Is Nothing AndAlso Document.Blocks.Count > 0 Then
                _NextBlock = Document.Blocks(0)
            End If
            If _NextBlock IsNot Nothing Then
                block = _NextBlock
                _NextBlock = block.NextBlock()
            End If
            If block IsNot Nothing AndAlso TypeOf block Is Paragraph Then
                Dim pDel As New ProcessParagraphDelegate(AddressOf ProcessParagraph)
                block.Dispatcher.BeginInvoke(pDel, System.Windows.Threading.DispatcherPriority.ApplicationIdle, CType(block, Paragraph))
            End If
            '_NextBlockIndex += 1

        End If
    End Sub

    Private Delegate Sub ProcessParagraphDelegate(ByRef paragraph As Paragraph)

    Private Sub _QueueTimer_Elapsed(ByVal sender As Object, ByVal e As System.Timers.ElapsedEventArgs) Handles _QueueTimer.Elapsed
        _QueueCount += 1
        If _QueueCount > 0 Then
            _QueueTimer.Stop()
            _QueueCount = 0
            Dim pDel As New ProcessNextBlockDelegate(AddressOf ProcessNextBlock)
            Dim args() As Object = Nothing
            Document.Dispatcher.BeginInvoke(pDel, System.Windows.Threading.DispatcherPriority.ApplicationIdle)
            _QueueTimer.Start()
        End If
    End Sub

    Public Sub ProcessAll()
        _QueueTimer.Stop()
        For i As Integer = 0 To Document.Blocks.Count - 1
            Dim Block As Block = Document.Blocks(i)
            If Block IsNot Nothing AndAlso TypeOf Block Is Paragraph Then
                ProcessParagraph(CType(Block, Paragraph))
            End If
        Next
        _QueueTimer.Start()
    End Sub


    Public Sub NewFile()

        WarnIfUnsaved()

        Me.Document = New FlowDocument
        Chunks.Clear()
        _NextBlock = Nothing

    End Sub

    Public Sub Save()
        If FileName <> "" Then
            Save(FileName)
        Else
            SaveAs()
        End If
    End Sub

    Public Sub SaveAs()

        Dim SaveFileDialog As New Microsoft.Win32.SaveFileDialog
        SaveFileDialog.Title = "Select location to save file to."
        If _Syntax.FileFilter <> "" Then
            SaveFileDialog.Filter = _Syntax.FileFilter
        End If
        If FileName <> "" Then
            SaveFileDialog.FileName = FileName
        End If

        SaveFileDialog.ShowDialog()

        FileName = SaveFileDialog.FileName

        Save(FileName)

    End Sub

    Public Sub Save(ByVal fileName As String)
        If fileName <> "" Then

            Dim fStream As New FileStream(fileName, FileMode.OpenOrCreate)
            Dim range As New TextRange(Document.ContentStart, Document.ContentEnd)
            range.Save(fStream, DataFormats.Text)
            fStream.Close()
            _ModifiedSinceLastSave = False

        End If
    End Sub

    Public Sub LoadFromFile()

        WarnIfUnsaved()

        Dim LoadFileDialog As New Microsoft.Win32.OpenFileDialog
        LoadFileDialog.Title = "Select file to open..."
        If _Syntax.FileFilter <> "" Then
            LoadFileDialog.Filter = _Syntax.FileFilter
        End If
        If FileName <> "" Then
            LoadFileDialog.FileName = FileName
        End If

        LoadFileDialog.ShowDialog()

        FileName = LoadFileDialog.FileName

        LoadFromFile(FileName)

    End Sub

    Public Sub WarnIfUnsaved()
        While _ModifiedSinceLastSave
            Dim dialogResult As MessageBoxResult = MessageBox.Show("This file has unsaved changes.  Save them?", "Oi, Plonker!", MessageBoxButton.YesNo)
            If dialogResult = MessageBoxResult.Yes Then
                SaveAs()
            Else
                Return
            End If
        End While
    End Sub

    Public Sub LoadFromFile(ByVal fileName As String)

        If fileName <> "" AndAlso File.Exists(fileName) Then

            Dim fStream As New FileStream(fileName, FileMode.OpenOrCreate)
            Dim range As New TextRange(Document.ContentStart, Document.ContentEnd)
            range.Load(fStream, DataFormats.Text)
            Chunks.Clear()
            _NextBlock = Nothing
            fStream.Close()
            ProcessAll()
            _ModifiedSinceLastSave = False

        End If

    End Sub

    Public Sub SetStartupPath()

        Dim LoadFileDialog As New Microsoft.Win32.OpenFileDialog
        LoadFileDialog.Title = "Select file to run..."
        LoadFileDialog.Filter = "All files (*.*)|*.*"
        If StartUpPath <> "" Then
            LoadFileDialog.FileName = StartUpPath
        End If

        LoadFileDialog.ShowDialog()

        If LoadFileDialog.FileName <> "" Then
            StartUpPath = LoadFileDialog.FileName
        End If

    End Sub

    Public Sub Run()
        If StartUpPath = "" Then
            SetStartupPath()
        End If
        If StartUpPath <> "" Then
            Dim process As New System.Diagnostics.Process
            Dim startInfo As New System.Diagnostics.ProcessStartInfo(StartUpPath)
            startInfo.UseShellExecute = True
            startInfo.WindowStyle = ProcessWindowStyle.Normal
            process.StartInfo = startInfo
            process.Start()
        End If
    End Sub

    Public Function GetParagraphTag(ByRef para As Paragraph) As CodeParagraphTag
        If para.Tag Is Nothing OrElse Not TypeOf para.Tag Is CodeParagraphTag OrElse CType(para.Tag, CodeParagraphTag).TagOf IsNot para Then
            Dim paraTag As New CodeParagraphTag
            paraTag.TagOf = para
            para.Tag = paraTag
        End If
        Return CType(para.Tag, CodeParagraphTag)
    End Function

    Public Sub GoToChunk(ByRef Chunk As CodeChunk)
        If Chunk IsNot Nothing AndAlso Chunk.DefinedAt IsNot Nothing Then
            Me.Focus()
            'CaretPosition = Chunk.DefinedAt.ContentEnd
            Selection.Select(Chunk.DefinedAt.ContentStart, Chunk.DefinedAt.ContentEnd)
        End If
    End Sub

    Public Sub AutoIndentAll()
        ProcessAll()
        For i As Integer = 0 To Document.Blocks.Count - 1
            Dim Block As Block = Document.Blocks(i)
            If TypeOf Block Is Paragraph Then
                AutoIndent(CType(Block, Paragraph))
            End If
        Next
        ProcessAll()
    End Sub

    Public Sub AutoIndent(ByVal paragraph As Paragraph)
        If paragraph IsNot Nothing AndAlso paragraph.Tag IsNot Nothing AndAlso TypeOf paragraph.Tag Is CodeParagraphTag Then
            Dim tag As CodeParagraphTag = paragraph.Tag
            Dim range As New TextRange(paragraph.ContentStart, paragraph.ContentEnd)
            Dim text As String = range.Text.TrimStart(vbTab)
            text = text.Trim()
            If text <> "" Then
                Dim indents As Integer = tag.IndentLevels
                If text.StartsWith("}") Then
                    indents -= 1
                End If
                Dim newText As String = ""
                newText += text
                'range.Text = newText
                paragraph.TextIndent = indents * IndentSpacing
            End If
        End If
    End Sub

    Public Sub RemovePrecedingWhitespace(ByVal paragraph As Paragraph)
        If Paragraph IsNot Nothing AndAlso Paragraph.Tag IsNot Nothing AndAlso TypeOf Paragraph.Tag Is CodeParagraphTag Then
            Dim tag As CodeParagraphTag = Paragraph.Tag
            Dim range As New TextRange(Paragraph.ContentStart, Paragraph.ContentEnd)
            Dim text As String = range.Text.TrimStart(vbTab)
            text = text.Trim()
            range.Text = text
        End If
    End Sub

    Public Sub TidyUpWhitespace()
        For i As Integer = 0 To Document.Blocks.Count - 1
            Dim Block As Block = Document.Blocks(i)
            If TypeOf Block Is Paragraph Then
                RemovePrecedingWhitespace(CType(Block, Paragraph))
            End If
        Next
    End Sub

End Class

Enum ParseState
    Normal
    StringLiteral
    Comment
End Enum