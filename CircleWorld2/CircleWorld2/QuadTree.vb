Option Strict On
Option Explicit On 

Public Module QuadTree

    Public MustInherit Class QuadTreeNode
        Public BoundingRectangleF As RectangleF
        Public Parent As QuadTreeNode
        Public smallestQuad As SizeF = New SizeF(30, 30)

        Public Sub New()
            BoundingRectangleF = New RectangleF()
        End Sub

        Public Sub New(ByVal bb As RectangleF)
            BoundingRectangleF = bb
        End Sub

        Public Function IntersectsWith(ByVal rect As RectangleF) As Boolean
            IntersectsWith = BoundingRectangleF.IntersectsWith(rect)
        End Function

        Public MustOverride Function GetCollisionList(ByVal obj As MovableObject, ByVal movementRect As RectangleF, ByVal g As Graphics) As ArrayList

        Public MustOverride Sub AddObject(ByVal obj As WorldObject)

        Public MustOverride Sub ResetObjects()

    End Class

    Public Class QuadTreeBranch
        Inherits QuadTreeNode

        Public UpperLeft As QuadTreeNode
        Public UpperRight As QuadTreeNode
        Public LowerLeft As QuadTreeNode
        Public LowerRight As QuadTreeNode

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal bb As RectangleF)
            MyBase.new(bb)
        End Sub

        Public Overrides Sub ResetObjects()
            If Not UpperLeft Is Nothing Then
                UpperLeft.ResetObjects()
            End If
            If Not UpperRight Is Nothing Then
                UpperRight.ResetObjects()
            End If
            If Not LowerLeft Is Nothing Then
                LowerLeft.ResetObjects()
            End If
            If Not LowerRight Is Nothing Then
                LowerRight.ResetObjects()
            End If
        End Sub


        Public Overrides Function GetCollisionList(ByVal obj As MovableObject, ByVal movementRect As RectangleF, ByVal g As Graphics) As ArrayList
            Dim ret As New ArrayList()

            If Not UpperLeft Is Nothing AndAlso UpperLeft.BoundingRectangleF.IntersectsWith(movementRect) Then
                ret.AddRange(UpperLeft.GetCollisionList(obj, movementRect, g))
            End If
            If Not UpperRight Is Nothing AndAlso UpperRight.BoundingRectangleF.IntersectsWith(movementRect) Then
                ret.AddRange(UpperRight.GetCollisionList(obj, movementRect, g))
            End If
            If Not LowerLeft Is Nothing AndAlso LowerLeft.BoundingRectangleF.IntersectsWith(movementRect) Then
                ret.AddRange(LowerLeft.GetCollisionList(obj, movementRect, g))
            End If
            If Not LowerRight Is Nothing AndAlso LowerRight.BoundingRectangleF.IntersectsWith(movementRect) Then
                ret.AddRange(LowerRight.GetCollisionList(obj, movementRect, g))
            End If

            Return ret
        End Function


        Public Overrides Sub AddObject(ByVal obj As WorldObject)
            Dim halfWidth As Single = BoundingRectangleF.Width / 2
            Dim halfHeight As Single = BoundingRectangleF.Height / 2
            Dim ulBnd As RectangleF = New RectangleF(BoundingRectangleF.X, BoundingRectangleF.Y, halfWidth, halfHeight)
            Dim urBnd As RectangleF = New RectangleF(BoundingRectangleF.X + halfWidth, BoundingRectangleF.Y, halfWidth, halfHeight)
            Dim llBnd As RectangleF = New RectangleF(BoundingRectangleF.X, BoundingRectangleF.Y + halfHeight, halfWidth, halfHeight)
            Dim lrBnd As RectangleF = New RectangleF(BoundingRectangleF.X + halfWidth, BoundingRectangleF.Y + halfHeight, halfWidth, halfHeight)

            If ulBnd.IntersectsWith(obj.BoundingRectangleF) Then
                If Not UpperLeft Is Nothing Then
                    UpperLeft.AddObject(obj)
                ElseIf halfWidth / 2 < smallestQuad.Width Or halfHeight / 2 < smallestQuad.Height Then
                    UpperLeft = New QuadTreeLeaf(ulBnd)
                    UpperLeft.AddObject(obj)
                Else
                    UpperLeft = New QuadTreeBranch(ulBnd)
                    UpperLeft.AddObject(obj)
                End If
            End If
            If urBnd.IntersectsWith(obj.BoundingRectangleF) Then
                If Not UpperRight Is Nothing Then
                    UpperRight.AddObject(obj)
                ElseIf halfWidth / 2 < smallestQuad.Width Or halfHeight / 2 < smallestQuad.Height Then
                    UpperRight = New QuadTreeLeaf(urBnd)
                    UpperRight.AddObject(obj)
                Else
                    UpperRight = New QuadTreeBranch(urBnd)
                    UpperRight.AddObject(obj)
                End If
            End If
            If llBnd.IntersectsWith(obj.BoundingRectangleF) Then
                If Not LowerLeft Is Nothing Then
                    LowerLeft.AddObject(obj)
                ElseIf halfWidth / 2 < smallestQuad.Width Or halfHeight / 2 < smallestQuad.Height Then
                    LowerLeft = New QuadTreeLeaf(llBnd)
                    LowerLeft.AddObject(obj)
                Else
                    LowerLeft = New QuadTreeBranch(llBnd)
                    LowerLeft.AddObject(obj)
                End If
            End If
            If lrBnd.IntersectsWith(obj.BoundingRectangleF) Then
                If Not LowerRight Is Nothing Then
                    LowerRight.AddObject(obj)
                ElseIf halfWidth / 2 < smallestQuad.Width Or halfHeight / 2 < smallestQuad.Height Then
                    LowerRight = New QuadTreeLeaf(lrBnd)
                    LowerRight.AddObject(obj)
                Else
                    LowerRight = New QuadTreeBranch(lrBnd)
                    LowerRight.AddObject(obj)
                End If
            End If
        End Sub
    End Class

    Public Class QuadTreeLeaf
        Inherits QuadTreeNode
        Private objCount As Integer = 0
        Public ObjectList() As WorldObject

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal bb As RectangleF)
            MyBase.new(bb)
        End Sub

        Public Overrides Sub ResetObjects()
            Dim wobj As WorldObject
            For Each wobj In ObjectList
                wobj.alreadyAdded = False
            Next
        End Sub

        Public Overrides Sub AddObject(ByVal obj As WorldObject)
            ReDim Preserve ObjectList(objCount)
            ObjectList(objCount) = obj
            objCount = objCount + 1
        End Sub

        Public Overrides Function GetCollisionList(ByVal obj As MovableObject, ByVal movementRect As RectangleF, ByVal g As Graphics) As ArrayList
            Dim ret As New ArrayList()
            Dim wobj As WorldObject

            For Each wobj In ObjectList
                If Not wobj.alreadyAdded Then
                    If wobj.BoundingRectangleF.IntersectsWith(movementRect) Then
                        If Not obj Is Nothing Then
                            Dim r1 As Region = obj.MovementProfileRegion
                            r1.Intersect(wobj.BoundingRegion)

                            'Dim d As DistanceF = New DistanceF(wobj.c.center, obj.c.center)
                            'If d.value < wobj.c.radius + obj.c.radius Then
                            If Not r1.IsEmpty(g) Then
                                wobj.alreadyAdded = True
                                ret.Add(wobj)
                            End If
                        Else
                            wobj.alreadyAdded = True
                            ret.Add(wobj)
                        End If
                    End If
                End If
            Next

            Return ret
        End Function

    End Class


End Module
