Imports SAPbouiCOM.Framework

Namespace AE_MSCM_V001
    Public Class ClsMain


        Private WithEvents SBO_Application As SAPbouiCOM.Application

        Sub New()
            SBO_Application = Application.SBO_Application
        End Sub



        Sub SBO_Application_MenuEvent(ByRef pVal As SAPbouiCOM.MenuEvent, ByRef BubbleEvent As Boolean) Handles SBO_Application.MenuEvent
            BubbleEvent = True

           

        End Sub

    End Class
End Namespace