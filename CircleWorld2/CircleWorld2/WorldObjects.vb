Option Strict On
Option Explicit On 

Public Module WorldObjects
    Private SpaceDivisor As Single = 2

    Public Enum WOTypeEnum
        WOT_CIRCLE = 0
        WOT_POLYGON
    End Enum

    Public Enum WOSubtypeEnum
        WOST_DEFAULT = 0
        WOST_TREE
    End Enum

    Public Class WorldObject

        Public alreadyAdded As Boolean

        Public c As CircleF
        Public p As Polygon

        Public type As WOTypeEnum = WOTypeEnum.WOT_CIRCLE

        Public subtype As WOSubtypeEnum = WOSubtypeEnum.WOST_DEFAULT

        Public Sub New()
            Me.c.center.X = 0
            Me.c.center.Y = 0
            Me.c.radius = 0
            type = WOTypeEnum.WOT_CIRCLE
        End Sub

        Public Sub New(ByVal c As PointF, ByVal r As Single)
            Me.c.center = c
            Me.c.radius = r
            type = WOTypeEnum.WOT_CIRCLE
        End Sub

        Public Sub New(ByVal p As Polygon)
            Me.p = p
            Me.c = p.CircumscribedCircleF
            type = WOTypeEnum.WOT_POLYGON
        End Sub

        Public Sub New(ByVal c As CircleF)
            Me.c = c
            type = WOTypeEnum.WOT_CIRCLE
        End Sub

        Public Sub New(ByVal c As CircleF, ByVal stype As WOSubtypeEnum)
            Me.c = c
            type = WOTypeEnum.WOT_CIRCLE
            subtype = stype
        End Sub

        Public ReadOnly Property BoundingRegion() As Region
            Get
                Dim pth As New Drawing2D.GraphicsPath()

                If Me.type = WOTypeEnum.WOT_CIRCLE Then
                    pth.AddEllipse(c.CircumscribedRectangleF)
                ElseIf Me.type = WOTypeEnum.WOT_POLYGON Then
                    pth.AddPolygon(p.Points)
                Else

                End If
                BoundingRegion = New Region(pth)
            End Get
        End Property

        Public Sub New(ByVal x As Single, ByVal y As Single, ByVal r As Single)
            Me.c.center.X = x
            Me.c.center.Y = y
            Me.c.radius = r
            type = WOTypeEnum.WOT_CIRCLE
        End Sub

        Public ReadOnly Property BoundingRectangleF() As RectangleF
            Get
                If Me.type = WOTypeEnum.WOT_CIRCLE Then
                    BoundingRectangleF = c.CircumscribedRectangleF
                ElseIf Me.type = WOTypeEnum.WOT_POLYGON Then
                    BoundingRectangleF = p.CircumscribedRectangleF
                Else
                    Err.Raise(666)
                End If
            End Get
        End Property

        Public Property X() As Single
            Get
                X = c.center.X
            End Get
            Set(ByVal Value As Single)
                c.center.X = Value
            End Set
        End Property

        Public Property Y() As Single
            Get
                Y = c.center.Y
            End Get
            Set(ByVal Value As Single)
                c.center.Y = Value
            End Set
        End Property

    End Class

    Public Class MovableObject
        Inherits WorldObject

        Private previousCenter As PointF

        Public facing As New PointF(0, -1)
        Public angularVelocity As Single
        Public velocity As PointF

        Public Sub New(ByVal center As PointF, ByVal r As Single)
            MyBase.New(center, r)
            previousCenter = center
        End Sub

        Public Sub New(ByVal x As Single, ByVal y As Single, ByVal r As Single)
            MyBase.New(x, y, r)
            previousCenter = c.center
        End Sub

        Public Sub New(ByVal circle As CircleF)
            MyBase.New(circle)
            previousCenter = circle.center
        End Sub

        Public ReadOnly Property Speed() As Single
            Get
                Speed = CSng(Math.Sqrt(Me.velocity.X() * Me.velocity.X() + Me.velocity.Y * Me.velocity.Y))
            End Get
        End Property

        Public Sub Halt()
            Me.velocity.X = 0
            Me.velocity.Y = 0
            Me.angularVelocity = 0
        End Sub


        Public Sub ForwardBackward(ByVal spd As Single)
            Me.velocity.X = Me.velocity.X + spd * Me.facing.X
            Me.velocity.Y = Me.velocity.Y + spd * Me.facing.Y
        End Sub

        Public Sub StrafeLeft(ByVal spd As Single)
            Me.velocity.X = Me.velocity.X + spd * Me.facing.Y
            Me.velocity.Y = Me.velocity.Y - spd * Me.facing.X
        End Sub

        Public Sub StrafeRight(ByVal spd As Single)
            Me.velocity.X = Me.velocity.X - spd * Me.facing.Y
            Me.velocity.Y = Me.velocity.Y + spd * Me.facing.X
        End Sub

        Public Function Move(ByVal ts As TimeSpan, ByVal qtRoot As QuadTreeBranch, ByVal g As Graphics) As RectangleF
            Dim movementRect As RectangleF

            movementRect = DoCollisionCheck(ts, qtRoot, g)

            Return movementRect
        End Function

        Public Function Move(ByVal ts As TimeSpan) As RectangleF
            Dim movementRect As RectangleF
            Dim angle As Double = Math.Atan2(Me.facing.Y, Me.facing.X)

            movementRect = Me.BoundingRectangleF
            previousCenter = Me.c.center

            angle = (angle + angularVelocity * ts.Ticks / TimeSpan.TicksPerMinute) Mod (TwoPI)
            Me.facing.X = CSng(Math.Cos(angle))
            Me.facing.Y = CSng(Math.Sin(angle))

            Me.X = Me.X + Me.velocity.X * ts.Ticks / TimeSpan.TicksPerMinute
            Me.Y = Me.Y + Me.velocity.Y * ts.Ticks / TimeSpan.TicksPerMinute

            movementRect = RectangleF.Union(movementRect, Me.BoundingRectangleF)

            Return movementRect
        End Function

        Private Function DoCollisionCheck(ByVal elapsed As TimeSpan, ByVal qtRoot As QuadTreeBranch, ByVal g As Graphics) As RectangleF
            Dim i As Integer
            Dim newElapsed As TimeSpan = New TimeSpan(elapsed.Ticks)
            Dim offset As TimeSpan
            Dim movementRect As RectangleF
            Dim plusMinus As Integer = -1

            movementRect = Me.Move(elapsed)
            If Me.IsTranslating Then
                Dim collist As ArrayList = qtRoot.GetCollisionList(Me, movementRect, g)
                If collist.Count > 0 Then
                    offset = New TimeSpan(CLng(newElapsed.Ticks / 2))
                    Debug.WriteLine("-------------")
                    Do While True
                        Debug.WriteLine(newElapsed.Ticks & " " & plusMinus & " " & offset.Ticks)
                        Me.UndoLastMove()
                        newElapsed = New TimeSpan(newElapsed.Ticks + (plusMinus * offset.Ticks))
                        If offset.Ticks < 1 Then
                            'as close as we can get; newElapsed contains the approx time of collision
                            movementRect = Me.Move(newElapsed)
                            Exit Do
                        End If

                        movementRect = Me.Move(newElapsed)
                        For i = 0 To collist.Count - 1
                            CType(collist(i), WorldObject).alreadyAdded = False
                        Next
                        collist = qtRoot.GetCollisionList(Me, movementRect, g)
                        offset = New TimeSpan(CLng(offset.Ticks / 2))
                        If collist.Count > 0 Then
                            plusMinus = -1
                        Else
                            plusMinus = 1
                        End If
                    Loop
                End If
                For i = 0 To collist.Count - 1
                    CType(collist(i), WorldObject).alreadyAdded = False
                Next
            End If
            Return movementRect
        End Function

        Public Function IsTranslating() As Boolean
            IsTranslating = Me.velocity.X <> 0 OrElse Me.velocity.Y <> 0
        End Function

        Public Function IsRotating() As Boolean
            IsRotating = Me.angularVelocity <> 0
        End Function

        Public Function IsMoving() As Boolean
            IsMoving = Me.IsTranslating OrElse Me.IsRotating
        End Function

        Public Sub UndoLastMove()
            Me.c.center = previousCenter
        End Sub

        Public Function MovementProfileRegion() As Region
            'return a region representing the area swept by the moving object
            Dim pth As New Drawing2D.GraphicsPath()

            If Me.velocity.Y <> 0 And Me.velocity.X <> 0 Then
                Dim movDir As Single = -CSng(Math.Atan2(Me.velocity.Y, Me.velocity.X))
                Dim prevXB As Single = Me.previousCenter.X + Me.c.radius * CSng(Math.Cos(PIOver2 - movDir))
                Dim prevYB As Single = Me.previousCenter.Y + Me.c.radius * CSng(Math.Sin(PIOver2 - movDir))
                Dim prevXT As Single = Me.previousCenter.X - Me.c.radius * CSng(Math.Cos(PIOver2 - movDir))
                Dim prevYT As Single = Me.previousCenter.Y - Me.c.radius * CSng(Math.Sin(PIOver2 - movDir))
                Dim newXB As Single = Me.c.X + Me.c.radius * CSng(Math.Sin(movDir))
                Dim newYB As Single = Me.c.Y + Me.c.radius * CSng(Math.Cos(movDir))
                Dim newXT As Single = Me.c.X - Me.c.radius * CSng(Math.Sin(movDir))
                Dim newYT As Single = Me.c.Y - Me.c.radius * CSng(Math.Cos(movDir))

                pth.AddLine(prevXB, prevYB, prevXT, prevYT)
                pth.AddLine(prevXT, prevYT, newXT, newYT)
                pth.AddArc(Me.c.CircumscribedRectangleF, 90 - movDir * OneEightyOverPI, -180)
                'pth.AddLine(newXT, newYT, newXB, newYB)
                pth.AddLine(newXB, newYB, prevXB, prevYB)

                MovementProfileRegion = New Region(pth)
            Else
                'MovementProfileRegion = New Region()
                MovementProfileRegion = Me.BoundingRegion
            End If
        End Function

    End Class

End Module
