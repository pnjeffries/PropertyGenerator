Public Class CodeParagraphTag

    Public Property IsBlockComment As Boolean = False
    Public Property IndentLevels As Integer = 0
    Public Property DeclarationOf As CodeChunk = Nothing
    Public Property TagOf As Paragraph = Nothing
    Public Property Context As CodeChunk = Nothing
    Public Property PreviousText As String = ""

End Class
