

create PROCEDURE [dbo].[AE_SP001_SOA_FY]   @BPFrom NVARCHAR(30),    @BPTo NVARCHAR(30),    @DateTo DATETIME,   @Condition Numeric(2)    AS   BEGIN   SET NOCOUNT ON   
DECLARE @DateFrom DATETIMEDECLARE @SalespersonFrom NVARCHAR(30)DECLARE @SalespersonTo NVARCHAR(30)DECLARE @Interval INT
SET @DateFrom = ''SET @SalespersonFrom = ''SET @SalespersonTo = ''SET @Interval = 30IF @DateTo IS NULL SET @DateTo = getdate()IF @DateFrom = @DateTo SET @DateFrom = ''IF @BPFrom IS NULL or @BPfrom = '*'	SET @BPFrom = ''IF @BPTo IS NULL or @BPTo = '*'	SET @BPTo = ''DECLARE @SysCurr NVARCHAR(3)DECLARE @LocCurr NVARCHAR(3)DECLARE @CompName NVARCHAR(MAX)		, @AliasName NVARCHAR(MAX)       ,@AddrLine1 NVARCHAR(MAX)       ,@AddrLine2 NVARCHAR(MAX)       ,@AddrLine3 NVARCHAR(MAX)       ,@AddrLine4 NVARCHAR(MAX)       ,@AddrLine5 NVARCHAR(MAX)       ,@AddrLine6 NVARCHAR(MAX)       ,@CompAdd NVARCHAR(MAX)       ,@CompTel NVARCHAR(MAX)       ,@CompFax NVARCHAR(MAX)       ,@CompRegNo NVARCHAR(MAX)       ,@GSTRegNo nvarchar(100)	
Declare @Street   varchar(50)  Declare @StreetNo   varchar(50)  Declare @Block   varchar(50)  Declare @ZipCode   varchar(50)  Declare @State   varchar(50)Declare @ComCountry varchar(50) Declare @ComEmail   varchar(50)
set @Street =  (select Top 1 ADM1.Street from ADM1)  set @StreetNo =  (select Top 1 ADM1.StreetNo from ADM1)  set @Block =  (select Top 1 ADM1.Block from ADM1)  set @ZipCode =  (select Top 1 ADM1.ZipCode from ADM1)  set @State =  (select b.Name from ADM1 a left outer join OCST b  on a.State=b.Code)  set @ComCountry = (select UPPER(d.Name) from OADM c left outer join OCRY d  on c.Country=d.Code) set @ComEmail =  (select Top 1 OADM.E_Mail from OADM)/* Retrieve Company Info */        

--SELECT Top 1 @LocCurr = MainCurncy

--			,@CoRegNo = TaxIdNum

--			,@GSTRegNo = TaxIdNum2 

--			,@CompName = ISNULL(OADM.PrintHeadr,OADM.CompnyName)

--FROM OADM
DECLARE  @Interval1 INT, @Interval2 INT        ,@Interval3 INT, @Interval4 INT         ,@Interval5 INTDECLARE  @Header1 NVARCHAR(20), @Header2 NVARCHAR(20)        ,@Header3 NVARCHAR(20), @Header4 NVARCHAR(20) 	,@Header5 NVARCHAR(20)
DECLARE  @Bracket1 INT, @Bracket2 INT        ,@Bracket3 INT, @Bracket4 INT         ,@Bracket5 INT
SELECT TOP 1     @SysCurr = SysCurrncy    ,@LocCurr = MainCurncy   ,@CompName = OADM.CompnyName   ,@AliasName=OADM.AliasName   ,@AddrLine4 = 'Tel: '+ COALESCE(RTRIM(Phone1),RTRIM(Phone2),'')+' Fax: '+ COALESCE(RTRIM(Fax),'')   ,@AddrLine5 = 'Company Reg No. ' + COALESCE(RTRIM(TaxIdNum),'')    ,@AddrLine6 = 'GST Reg No. ' + COALESCE(RTRIM(TaxIdNum2),'')    ,@CompAdd = COMPNYADDR   ,@CompTel = PHONE1   ,@CompFax = FAX   ,@GSTRegNo = TaxIdNum2    ,@CompRegNo = TaxIdNumFROM OADMSELECT TOP 1     @AddrLine1 = Street    ,@AddrLine2 = Building   ,@AddrLine3 = COALESCE(RTRIM((SELECT OCRY.[Name] FROM OCRY WHERE OCRY.Code = Country)),'')                + ' ' +COALESCE(RTRIM(ZipCode),'')

  FROM ADM1



IF @Interval = 15 

 BEGIN

    SELECT 

      @Interval1 = 15

     ,@Interval2 = 15

     ,@Interval3 = 15

     ,@Interval4 = 15

     ,@Interval5 = 9999999

 END

ELSE IF @Interval = 30 

      BEGIN  

        SELECT 

          @Interval1 = 30

         ,@Interval2 = 30

         ,@Interval3 = 30

         ,@Interval4 = 30

         ,@Interval5 = 9999999

      END

     ELSE IF @Interval = 90 

           BEGIN 

             SELECT 

               @Interval1 = 90

              ,@Interval2 = 90

              ,@Interval3 = 90

              ,@Interval4 = 90

              ,@Interval5 = 9999999

           END 



SET @Bracket1 = @Interval1

SET @Bracket2 = @Bracket1 + @Interval2

SET @Bracket3 = @Bracket2 + @Interval3

SET @Bracket4 = @Bracket3 + @Interval4

SET @Bracket5 = @Bracket4 + @Interval5



SET @Header1 = '1 to '+ RTRIM(@Bracket1) + ' Days' 

SET @Header2 = RTRIM(@Bracket1+1) + ' to ' + RTRIM(@Bracket2) + ' Days'

SET @Header3 = RTRIM(@Bracket2+1) + ' to ' + RTRIM(@Bracket3) + ' Days'

SET @Header4 = RTRIM(@Bracket3+1) + ' to ' + RTRIM(@Bracket4) + ' Days'

SET @Header5 = 'Above ' + RTRIM(@Bracket4) + ' Days'





/* Get Reconciliation Sum base on BP */

SELECT 

	ITR1.TransID,ITR1.TransRowId, IsCredit,MAX(ReconDate) AS ReconDate, 

	SUM(ISNULL(ITR1.ReconSum * CASE WHEN IsCredit='C' THEN 1 ELSE -1 END,0))  AS ReconSum,

	SUM(ISNULL(ITR1.ReconSumFC * CASE WHEN IsCredit='C' THEN 1 ELSE -1 END,0))  AS ReconSumFC

INTO #RECON

FROM OITR AS OITR INNER JOIN ITR1 AS ITR1 ON OITR.ReconNum=ITR1.ReconNum 

WHERE OITR.Canceled<>'Y' AND OITR.ReconDate<=@DateTo    AND OITR.ReconType NOT IN(7)

        AND (ISNULL(@BPFrom,'') = '' OR ITR1.ShortName >= @BPFrom)

        AND (ISNULL(@BPTo,'') = '' OR ITR1.ShortName <= @BPTo)

GROUP BY ITR1.TransID,ITR1.TransRowId,IsCredit





/*General Ledger*/

SELECT

   T0.TransId

  ,CASE WHEN T0.TransType = 13 THEN T3.DocNum

        WHEN T0.TransType = 14 THEN T4.DocNum

        WHEN T0.TransType = 24 THEN T7.DocNum

		WHEN T0.TransType = 30 THEN OJDT.Number

        ELSE ''

   END  AS DocNum

  ,T0.Ref2

  ,CASE WHEN T0.TransType = 13 THEN ISNULL(T3.NumAtCard,T0.Ref2)

        WHEN T0.TransType = 14 THEN T4.NumAtCard --ISNULL(T4.NumAtCard,T0.Ref2)

        WHEN T0.TransType = 24 THEN T7.CounterRef --ISNULL(,T0.Ref2)

        ELSE T0.Ref2

   END AS CustomerRef



  ,CASE WHEN T0.TransType = 13 THEN CASE WHEN T3.DocRate > 0 THEN T3.DocRate ELSE 1 END

        WHEN T0.TransType = 14 THEN CASE WHEN T4.DocRate > 0 THEN T4.DocRate ELSE 1 END

        WHEN T0.TransType = 24 THEN CASE WHEN T7.DocRate > 0 THEN T7.DocRate ELSE 1 END

        ELSE CASE WHEN T0.SystemRate > 0 THEN T0.SystemRate ELSE 1 END

   END AS DocRate

  ,CASE WHEN T0.TransType = 13 THEN 'INV'

        WHEN T0.TransType = 14 THEN 'CN'

        WHEN T0.TransType = 24 THEN 'PYMT'

		WHEN T0.TransType = 30 THEN 'JE'

        ELSE ''

   END AS 'SERIES'

  ,CASE WHEN T0.TransType = 13 THEN T3.NumAtCard

        WHEN T0.TransType = 14 THEN T4.NumAtCard

        WHEN T0.TransType = 24 THEN T7.CounterRef

		WHEN T0.TransType = 30 THEN OJDT.Memo

        ELSE ''

   END AS 'References'   ,CASE WHEN T0.TransType = 13 THEN inv.Remark 

        WHEN T0.TransType = 14 THEN cn.Remark 

        WHEN T0.TransType = 24 THEN pay.Remark 

		WHEN T0.TransType = 30 THEN JE.Remark  

        ELSE ''

   END AS 'SeriesRemark'

  ,CASE WHEN T0.TransType = 13 THEN T3.Canceled

        WHEN T0.TransType = 14 THEN T4.Canceled

        WHEN T0.TransType = 24 THEN T7.Canceled

		WHEN T0.TransType = 30 THEN 'N'

   END AS 'Canceled'

--  ,COALESCE(T3.SlpCode, T4.SlpCode, T2.SlpCode) AS SlpCode

--  ,COALESCE(T5.SlpName, T6.SlpName, T2.SlpName)AS SlpName  

  ,T0.ShortName AS CardCode

  ,T0.ShortName AS TempCardCode

  ,COALESCE(T1.CardName, T3.CardName, T4.CardName)AS CardName  

  ,T0.TransType AS TransType

  ,T0.CreatedBy AS DocEntry

  ,T0.DueDate AS DueDate

  ,T0.RefDate AS DocDate

  ,T0.BaseRef AS Reference

  ,T0.RevSource

  ,ISNULL(OJDT2.StorNoTotr ,OJDT.StorNoTotr ) AS StorNoTotr 

  ,ISNULL(T0.Debit,0) AS Debit

  ,ISNULL(T0.FcDebit,0) AS DebitFC

  ,ISNULL(T0.Credit,0) AS Credit

  ,ISNULL(T0.FcCredit,0) AS CreditFC

  ,ISNULL(T0.Debit,0) - ISNULL(T0.Credit,0) + (ISNULL(#RECON.ReconSum,0)* CASE WHEN ISNULL(T0.Credit,0)=0 THEN 1 ELSE -1 end)AS Balance

  ,ISNULL(T0.FcDebit,0) - ISNULL(T0.FcCredit,0) + (ISNULL(#RECON.ReconSumFC,0)*CASE WHEN ISNULL(T0.FcCredit,0)=0 THEN 1 ELSE -1 END) AS BalanceFC

--  ,ISNULL(T0.Debit,0) - ISNULL(T0.Credit,0)  AS Balance+ ISNULL(#RECON.ReconSum,0)

--  ,ISNULL(T0.FcDebit,0) - ISNULL(T0.FcCredit,0)  AS BalanceFC + ISNULL(#RECON.ReconSumFC,0)

  ,ISNULL(T0.Debit,0) - ISNULL(T0.Credit,0)  AS LCAmount

  ,ISNULL(T0.FcDebit,0) - ISNULL(T0.FcCredit,0)  AS FCAmount

  ,ISNULL(#RECON.ReconSum,0) AS ReconSum

  ,ISNULL(#RECON.ReconSumFC,0)  AS ReconSumFC

  ,CASE WHEN T0.RevSource='F' OR ISNULL(T0.FcDebit,0)=0 AND  ISNULL(T0.FcCredit,0)=0 THEN @LocCurr ELSE ISNULL(UPPER(T0.FcCurrency),@LocCurr) END AS DocCurrency

  ,CASE WHEN ISNULL(T0.FcDebit,0)=0 AND  ISNULL(T0.FcCredit,0)=0

        THEN ISNULL(T0.Debit,0) - ISNULL(T0.Credit,0) + ISNULL(#RECON.ReconSum,0)

        ELSE ISNULL(T0.FcDebit,0) - ISNULL(T0.FcCredit,0) + ISNULL(#RECON.ReconSumFC,0)

   END AS TransAmt

  ,T0.IntrnMatch

  ,ISNULL(#RECON.ReconDate,T0.MthDate) AS MthDate

  ,T0.Line_Id

  ,T1.CreditLine AS CreditLimit

  ,T1.Currency AS [currency]

  ,CASE WHEN T0.TransType = 13 THEN T3.GroupNum 

        WHEN T0.TransType = 14 THEN T4.GroupNum

        ELSE T1.GroupNum

   END AS PaymentGroup

  ,OCTG.PymntGroup AS PaymentTerms

  ,COALESCE(T1.Phone1,T1.Phone2,'') AS Telephone

  ,ISNULL(T1.Fax,'') AS Fax

  ,ISNULL(T1.CntctPrsn,'') AS ContactPerson

  ,ISNULL(T1.Address,'') AS Address

  ,ISNULL(T1.City,'') AS City

  ,ISNULL((SELECT OCRY.[Name] FROM OCRY WHERE OCRY.Code = T1.Country),'') AS CountryZ

  ,ISNULL(T1.ZipCode,'') AS ZipCode  

  ,ISNULL(X2.Address,'') AS BillToAddress

  ,ISNULL(X2.Street,'') AS BillToStreet

  ,ISNULL(X2.StreetNo,'') AS BillToStreetNo

  ,ISNULL(X2.State,'') AS BillToState

  ,ISNULL(X2.Block,'') AS BillToBlock

  ,ISNULL(X2.City,'') AS BillToCity

  ,ISNULL(X2.Building,'') AS BillBuilding

  ,COALESCE(RTRIM((SELECT Name FROM OCST WHERE Code = State)),'') AS BillState

  --,ISNULL((SELECT OCRY.[Name] FROM OCRY WHERE OCRY.Code = X2.Country),'') AS BillToCountry    ,ISNULL((SELECT Top 1 OCRY.[Name] FROM OCRY WHERE OCRY.Code = X2.Country),'') AS BillToCountry

  ,ISNULL(X2.ZipCode,'') AS BillToZipCode  

  INTO #GL

  FROM JDT1 T0 INNER JOIN (OCRD T1 INNER JOIN OSLP T2 ON T1.SlpCode = T2.SlpCode

                                   LEFT OUTER JOIN CRD1 X2  ON T1.CardCode = X2.CardCode AND X2.Address = T1.BillToDef 

                                      AND X2.AdresType = 'B') 

                 ON T0.ShortName = T1.CardCode and T1.CardType = 'C'

               LEFT OUTER JOIN (OINV T3 INNER JOIN OSLP T5 ON T3.SlpCode = T5.SlpCode) ON T0.TransType = 13 AND T0.CreatedBy = T3.DocEntry      

               LEFT OUTER JOIN (ORIN T4  INNER JOIN OSLP T6 ON T4.SlpCode = T6.SlpCode) ON T0.TransType = 14 AND T0.CreatedBy = T4.DocEntry              

				LEFT OUTER JOIN ORCT T7 ON T0.TransType = 24 AND T0.CreatedBy = T7.DocEntry

   				INNER JOIN OJDT ON OJDT.TransID=T0.TransId

				LEFT OUTER JOIN OCRD C1 ON C1.CardCode=T0.ShortName 

				LEFT OUTER JOIN OCTG OCTG ON OCTG.GroupNum=C1.GroupNum

			LEFT OUTER JOIN #RECON ON #RECON.TransID=T0.TransID AND T0.Line_ID=#RECON.TransRowId

	LEFT OUTER JOIN OJDT AS OJDT2 ON OJDT2.StorNoTotr=T0.TransID AND ISNULL(OJDT2.StorNoTotr,'')<>''	left outer join nnm1 INV on INV.Series = T3.Series 	left outer join nnm1 CN on cn.Series = t4.Series 	left outer join nnm1 PAY on PAY.Series = T7.Series 	left outer join nnm1 JE on JE.Series =  OJDT.Series 	
  WHERE 

--T0.TransID IN(43711,33331) and

--T0.TransID=43711 and

--T0.RefDate = @DateTo

		T0.RefDate <= @DateTo AND

         (ISNULL(@BPFrom,'') = '' OR T0.ShortName >= @BPFrom)

        AND (ISNULL(@BPTo,'') = '' OR T0.ShortName <= @BPTo)

        AND (ISNULL(@SalespersonFrom,'') = '' OR COALESCE(T5.SlpName, T6.SlpName, T2.SlpName) >= @SalespersonFrom)

        AND (ISNULL(@SalespersonTo,'') = '' OR COALESCE(T5.SlpName, T6.SlpName, T2.SlpName) <= @SalespersonTo)

AND ISNULL(ISNULL(OJDT2.StorNoTotr ,OJDT.StorNoTotr ),'')=''



--select * from #GL



/*Open Items in General Ledger that ha been paid once and reconciled*/

SELECT * 

INTO #GL_OPEN 

FROM #GL 

WHERE Canceled<>'Y' AND (TransAmt<>0)    --(IntrnMatch = 0 or MthDate > @DateTo)   --AND 





/*Fully Paid but Unreconciled Invoices and Payments*/

SELECT  T2.TransId

       ,T0.DocNum AS PaymentNum

       ,T1.InvType

       ,T1.DocEntry As DocEntry

       ,T0.DocCurr

       ,ISNULL(T1.SumApplied,0) AS LCPaymentAmount

       ,ISNULL(T1.AppliedFC,0) AS FCPaymentAmount

       ,T0.CardCode

       ,T2.MthDate

       ,T1.DocLine AS LineNum

  INTO #APPLIED_PAYMENT

  FROM RCT2 T1 INNER JOIN ORCT T0 ON T1.DocNum = T0.DocNum

               INNER JOIN JDT1 T2 ON T1.DocEntry = T2.CreatedBy AND T1.InvType = T2.TransType AND T2.ShortName <> T2.Account

  WHERE (@DateTo = '' or T0.DocDate <= @DateTo) AND T0.CardCode = T2.ShortName

         AND T0.CardCode IN (SELECT DISTINCT CardCode FROM #GL_OPEN) 

         AND T0.Canceled = 'N'



--select '285',* from #APPLIED_PAYMENT



--select '247' as '247', * from #APPLIED_PAYMENT



/*Unknown Transaction*/

SELECT DISTINCT CARDCODE INTO #NOT_APPLICABLE FROM #APPLIED_PAYMENT WHERE InvType NOT IN (13, 14, 30)



/* Apply to AR Invoices */

UPDATE #GL_OPEN 

  SET LCAmount = ISNULL(LCAmount,0) - ISNULL(#APPLIED_PAYMENT.LCPaymentAmount,0)

     ,FCAmount = ISNULL(FCAmount,0) - ISNULL(#APPLIED_PAYMENT.FCPaymentAmount,0)

  FROM #GL_OPEN LEFT OUTER JOIN 

                 (SELECT  DocEntry

                         ,SUM(LCPaymentAmount) AS LCPaymentAmount

                         ,SUM(FCPaymentAmount) AS FCPaymentAmount

                     FROM #APPLIED_PAYMENT 

                     WHERE InvType = 13 

                     GROUP BY DocEntry) #APPLIED_PAYMENT ON #GL_OPEN.DocEntry = #APPLIED_PAYMENT.DocEntry

  WHERE TransType = 13 AND #GL_OPEN.CardCode NOT IN (SELECT CardCode FROM #NOT_APPLICABLE)





UPDATE #GL_OPEN 

  SET LCAmount = ISNULL(LCAmount,0) - ISNULL(#APPLIED_PAYMENT.LCPaymentAmount,0)

     ,FCAmount = ISNULL(FCAmount,0) - ISNULL(#APPLIED_PAYMENT.FCPaymentAmount,0)

  FROM #GL_OPEN LEFT OUTER JOIN 

                 (SELECT  DocEntry

                         ,SUM(LCPaymentAmount) AS LCPaymentAmount

                         ,SUM(FCPaymentAmount) AS FCPaymentAmount

                     FROM #APPLIED_PAYMENT 

                     WHERE InvType = 30 

                     GROUP BY DocEntry, LineNum) #APPLIED_PAYMENT ON #GL_OPEN.DocEntry = #APPLIED_PAYMENT.DocEntry

  WHERE TransType = 30 AND #GL_OPEN.CardCode NOT IN (SELECT CardCode FROM #NOT_APPLICABLE)



--select '315', * from #GL_OPEN





/* Apply to AR Credit Note */

UPDATE #GL_OPEN 

  SET LCAmount = ISNULL(LCAmount,0) + ISNULL(#APPLIED_PAYMENT.LCPaymentAmount,0)

     ,FCAmount = ISNULL(FCAmount,0) + ISNULL(#APPLIED_PAYMENT.FCPaymentAmount,0)

  FROM #GL_OPEN LEFT OUTER JOIN 

                 (SELECT  DocEntry

                         ,SUM(LCPaymentAmount) AS LCPaymentAmount

                         ,SUM(FCPaymentAmount) AS FCPaymentAmount

                     FROM #APPLIED_PAYMENT 

                     WHERE InvType = 14 

                     GROUP BY DocEntry) #APPLIED_PAYMENT ON #GL_OPEN.DocEntry = #APPLIED_PAYMENT.DocEntry

  WHERE TransType = 14 AND #GL_OPEN.CardCode NOT IN (SELECT CardCode FROM #NOT_APPLICABLE)


UPDATE #GL_OPEN 

  SET LCAmount = ISNULL(LCAmount,0) + ISNULL(#APPLIED_PAYMENT.LCPaymentAmount,0)

     ,FCAmount = ISNULL(FCAmount,0) + ISNULL(#APPLIED_PAYMENT.FCPaymentAmount,0)

  FROM #GL_OPEN LEFT OUTER JOIN 

                 (SELECT  PaymentNum

                         ,SUM(CASE InvType WHEN 13 THEN 1 WHEN 14 THEN -1 WHEN 30 THEN 1 END * LCPaymentAmount) AS LCPaymentAmount

                         ,SUM(CASE InvType WHEN 13 THEN 1 WHEN 14 THEN -1 WHEN 30 THEN 1 END * FCPaymentAmount) AS FCPaymentAmount

                     FROM #APPLIED_PAYMENT 

                     GROUP BY PaymentNum) #APPLIED_PAYMENT ON #GL_OPEN.DocEntry = #APPLIED_PAYMENT.PaymentNum

  WHERE TransType = 24 AND #GL_OPEN.CardCode NOT IN (SELECT CardCode FROM #NOT_APPLICABLE)


DELETE FROM #GL_OPEN WHERE TransAmt = 0
DECLARE  @CardCode NVARCHAR(30)

        ,@PaymentNum NVARCHAR(40)

        ,@CurPaymentNum NVARCHAR(40)

        ,@PaymentDate DATETIME

        ,@PaymentDocRate NVARCHAR(40)

        ,@PaymentReference NVARCHAR(60)

        ,@PaymentCurrency NVARCHAR(3)

        ,@PaymentMeans NVARCHAR(100)

        ,@CashAmount NVARCHAR(40)

        ,@CheckAmount NVARCHAR(40)

        ,@CreditCardAmount NVARCHAR(40)

        ,@TransferAmount NVARCHAR(40)

        ,@TransferAmountFC NVARCHAR(40)

        ,@DocNum NVARCHAR(40)

        ,@DocDate DATETIME

        ,@CustomerRef NVARCHAR(60)

        ,@DocCur NVARCHAR(3)

        ,@DocRate NVARCHAR(40)

        ,@DocTotal NVARCHAR(40)

        ,@DocTotalFC NVARCHAR(40)

        ,@DocType NVARCHAR(40)

        ,@PaidAmount NVARCHAR(40)

        ,@PaidAmountFC NVARCHAR(40)

        ,@F_RefDate DATETIME

        ,@T_RefDate DATETIME



SELECT @F_RefDate = F_RefDate, @T_RefDate = T_RefDate 

  FROM OFPR WHERE CONVERT(NVARCHAR(30),@DateTo,112) BETWEEN F_RefDate AND T_RefDate 



IF @T_RefDate > @DateTo SET @T_RefDate = @DateTo



SELECT CONVERT(NVARCHAR(30),'') AS CardCode

      ,CONVERT(NVARCHAR(MAX),'') AS PaymentDetails 

  INTO #PAYMENT_DETAILS



DECLARE Payment CURSOR FOR 

SELECT DISTINCT CardCode 

  FROM #GL_OPEN

OPEN Payment

FETCH NEXT FROM Payment

INTO @CardCode 

WHILE @@FETCH_STATUS = 0 

  BEGIN

    INSERT INTO #PAYMENT_DETAILS

    SELECT @CardCode, CONVERT(NVARCHAR(MAX),'') AS PaymentDetails



    DECLARE PaidDoc CURSOR FOR 

    SELECT 

      ISNULL(CONVERT(NVARCHAR(30),X0.DocNum),'') AS PaymentNo

     ,ISNULL(X0.DocDate,'') AS PaymentDate

     ,ISNULL(CONVERT(NVARCHAR(60),COALESCE((CASE X0.DocRate WHEN 0 THEN 1 ELSE X0.DocRate END),

                                           (CASE X3.DocRate WHEN 0 THEN 1 ELSE X3.DocRate END))),'') AS PaymentDocRate

     ,ISNULL(CONVERT(NVARCHAR(60),COALESCE(X0.CounterRef,X0.Ref2)),'') AS PaymentReference

     ,ISNULL(CONVERT(NVARCHAR(3),X0.DocCurr),'') AS PaymentCurrency

     ,ISNULL(CASE WHEN X0.CashSum <> 0 THEN 'CASH'

           WHEN X0.CreditSum <> 0 THEN 'CREDIT CARD'

           WHEN X0.[CheckSum] <> 0 THEN 'CHECK'

           WHEN X0.TrsfrSum <> 0 THEN 'BANK TRANSFER'

      END,'') AS PaymentMeans

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X0.CashSum)),'') AS CashAmount

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X0.[CheckSum])),'') AS CheckAmount

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X0.CreditSum)),'') AS CreditCardAmount

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X0.TrsfrSum)),'') AS TransferAmount

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X0.TrsfrSumFC)),'') AS TransferAmountFC

     ,ISNULL(CONVERT(NVARCHAR(40),COALESCE(Y1.DocNum,Y2.DocNum)),'') AS DocNum

     ,ISNULL(COALESCE(Y1.DocDate,Y2.DocDate),'') AS DocDate

     ,ISNULL(CONVERT(NVARCHAR(60),COALESCE(Y1.NumAtCard,Y2.NumAtCard)),'')  AS CustomerRef

     ,ISNULL(CONVERT(NVARCHAR(3),COALESCE(Y1.DocCur,Y2.DocCur)),'') AS DocCur

     ,ISNULL(CONVERT(NVARCHAR(40),COALESCE((CASE Y1.DocRate WHEN 0 THEN 1 ELSE Y1.DocRate END),

                                                    (CASE Y2.DocRate WHEN 0 THEN 1 ELSE Y2.DocRate END))),'') AS DocRate

     ,ISNULL(CONVERT(NVARCHAR(40),COALESCE(CONVERT(DECIMAL(19,2),Y1.DocTotal),CONVERT(DECIMAL(19,2),Y2.DocTotal))),'') AS DocTotal

     ,ISNULL(CONVERT(NVARCHAR(40),COALESCE(CONVERT(DECIMAL(19,2),Y1.DocTotalFC),CONVERT(DECIMAL(19,2),Y2.DocTotalFC))),'') AS DocTotalFC

     ,CASE WHEN ISNULL(X3.InvType, X0.ObjType) = 13 THEN 'IN'

           WHEN ISNULL(X3.InvType, X0.ObjType) = 14 THEN 'CN'

           WHEN ISNULL(X3.InvType, X0.ObjType) = 24 THEN 'RC'

           WHEN ISNULL(X3.InvType, X0.ObjType) = 30 THEN 'JE'

      END AS DocType    

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X3.SumApplied)),'') AS PaidAmount

     ,ISNULL(CONVERT(NVARCHAR(40),CONVERT(DECIMAL(19,2),X3.AppliedFC)),'') AS PaidAmountFC

     FROM ORCT X0 LEFT OUTER JOIN (RCT2 X3 LEFT OUTER JOIN OINV Y1 ON X3.DocEntry = Y1.DocEntry AND X3.InvType = 13

                                           LEFT OUTER JOIN ORIN Y2 ON X3.DocEntry = Y2.DocEntry AND X3.InvType = 14

                                   ) ON X0.DocNum =X3.DocNum

     WHERE X0.Canceled = 'N' AND X0.CardCode = @CardCode 

          AND (X0.DocDate BETWEEN @F_RefDate AND @T_RefDate)

     ORDER BY X0.DocDate, X0.DocNum, X3.DocEntry, COALESCE(Y1.DocDate,Y2.DocDate)     

     OPEN PaidDoc

     FETCH NEXT FROM PaidDoc

     INTO @CurPaymentNum, @PaymentDate, @PaymentDocRate, @PaymentReference

         ,@PaymentCurrency, @PaymentMeans, @CashAmount, @CheckAmount, @CreditCardAmount

         ,@TransferAmount, @TransferAmountFC, @DocNum, @DocDate, @CustomerRef

         ,@DocCur, @DocRate, @DocTotal, @DocTotalFC, @DocType, @PaidAmount, @PaidAmountFC

     WHILE @@FETCH_STATUS = 0 

       BEGIN

         IF @PaymentNum = @CurPaymentNum          

           BEGIN   

            IF @DocNum <> ''

             BEGIN          

             UPDATE #PAYMENT_DETAILS

               SET PaymentDetails = ISNULl(PaymentDetails,'')+CHAR(9)+

                                   +@DocType

                                   +' Document No.: '+@DocNum 

                                   +' Document Date: '+RTRIM(CONVERT(NVARCHAR(60),@DocDate,102))

                                   +' Document Reference: '+@CustomerRef 

                                   +' Document Rate: '+@DocRate 

                                   +' Document Amount: '

                                   +' '+(CASE WHEN @DocCur <> @LocCurr THEN @DocCur+ ' '+ @DocTotalFC

                                              ELSE @LocCurr+ ' '+ @DocTotal    

                                         END)

                                   +' Paid Amount: '+(CASE WHEN @DocCur <> @LocCurr THEN @PaymentCurrency +' '+@PaidAmountFC

                                                                                  ELSE @LocCurr+ ' '+ @PaidAmount

                                                                             END)

                                   +CHAR(13)

               WHERE CardCode = @CardCode

             END

           END

         ELSE

           BEGIN

            IF @DocNum <> ''

             BEGIN

             UPDATE #PAYMENT_DETAILS             

               SET PaymentDetails = ISNULl(PaymentDetails,'')

                                   +'Payment No.: '+@CurPaymentNum 

                                   +' Payment Date: '+RTRIM(CONVERT(NVARCHAR(60),@PaymentDate,102))

                                   +' Paid by: '+@PaymentMeans

                                   +' Payment Reference: '+@PaymentReference

                                   +' Payment Rate: '+@PaymentDocRate 

                                   +' Payment Amount: '+@PaymentCurrency

                                   +' '+(CASE @PaymentMeans WHEN 'CASH' THEN @CashAmount

                                                            WHEN 'CREDIT CARD' THEN @CreditCardAmount

                                                            WHEN 'CHECK' THEN @CheckAmount

                                                            WHEN 'BANK TRANSFER' THEN @TransferAmount

                                         END)+CHAR(13)+CHAR(9)+

                                   +@DocType

                                   +' Document No.: '+@DocNum 

                                   +' Document Date: '+RTRIM(CONVERT(NVARCHAR(60),@DocDate,102))

                                   +' Document Reference: '+@CustomerRef 

                                   +' Document Rate: '+@DocRate 

                                   +' Document Amount: '

                                   +' '+(CASE WHEN @DocCur <> @LocCurr THEN @DocCur+ ' '+ @DocTotalFC

                                            ELSE @LocCurr+ ' '+ @DocTotal    

                                         END)

                                   +' Paid Amount: '+(CASE WHEN @DocCur <> @LocCurr THEN @PaymentCurrency +' '+@PaidAmountFC

                                                                                  ELSE @LocCurr+ ' '+ @PaidAmount

                                                                             END)

                                   +CHAR(13)

               WHERE CardCode = @CardCode

             END 

            ELSE

             BEGIN

             UPDATE #PAYMENT_DETAILS             

               SET PaymentDetails = ISNULl(PaymentDetails,'')

                                   +'Payment No.: '+@CurPaymentNum 

                                   +' Payment Date: '+RTRIM(CONVERT(NVARCHAR(60),@PaymentDate,102))

                                   +' Paid by: '+@PaymentMeans

                                   +' Payment Reference: '+@PaymentReference

                                   +' Payment Rate: '+@PaymentDocRate 

                                   +' Payment Amount: '+@PaymentCurrency

                                   +' '+(CASE @PaymentMeans WHEN 'CASH' THEN @CashAmount

                                                            WHEN 'CREDIT CARD' THEN @CreditCardAmount

                                                            WHEN 'CHECK' THEN @CheckAmount

                                                            WHEN 'BANK TRANSFER' THEN @TransferAmount

                                         END)+CHAR(13)

               WHERE CardCode = @CardCode               

             END 

           END 

         SET @PaymentNum = @CurPaymentNum

         SET @CurPaymentNum = NULL

         FETCH NEXT FROM PaidDoc

         INTO @CurPaymentNum, @PaymentDate, @PaymentDocRate, @PaymentReference

         ,@PaymentCurrency, @PaymentMeans, @CashAmount, @CheckAmount, @CreditCardAmount 

         ,@TransferAmount, @TransferAmountFC, @DocNum, @DocDate, @CustomerRef

         ,@DocCur, @DocRate, @DocTotal, @DocTotalFC, @DocType, @PaidAmount, @PaidAmountFC          

       END 

     CLOSE PaidDoc

     DEALLOCATE PaidDoc

           

     SET @CardCode = NULL

     FETCH NEXT FROM Payment

     INTO @CardCode 

  END

CLOSE Payment

DEALLOCATE Payment



/* Create a temp table for contact details 

	Special For HIsaka

*/

SELECT     

	OCPR.CardCode, OCPR.Name AS ContactName,OCPR.Tel1 AS ContactTel, OCPR.Fax AS ContactFax 

INTO #ContactDtls

FROM         OCRD  INNER JOIN

		  OCPR ON OCRD.CardCode = OCPR.CardCode 

WHERE OCPR.Notes1='Accounts Dept' AND OCRD.CardType='C'

	AND OCPR.CntctCode=(SELECT MAX(CntctCode) FROM OCPR WHERE OCPR.CardCode=OCRD.CardCode AND Notes1='Accounts Dept')



if @condition = 1  

  Begin

    SELECT  #GL_OPEN.CardCode, #GL_OPEN.CardName,sum(isnull(#GL_OPEN.LCAmount,0)) [Balance],

	case when isnull(d.E_Mail,'') = '' and isnull(d1.E_MailL,'') = '' then ''

	     when isnull(d.E_Mail,'') = '' then replace(d1.E_MailL,';',',') 

	     when isnull(d1.E_MailL,'') = '' then replace(d.E_Mail,';',',') 

		 when isnull(d.E_Mail,'') <> '' and isnull(d1.E_MailL,'') <> '' then replace(d.E_Mail,';',',') + ',' + replace(d1.E_MailL ,';',',')

		 end [Free_Text] 

	   FROM #GL_OPEN LEFT OUTER JOIN

	OCRD d ON d.CardCode=#GL_OPEN.CardCode

	join OCPR d1 on d.CardCode = d1.CardCode

	GROUP BY #GL_OPEN.CardCode, #GL_OPEN.CardName, d.E_Mail , d1.E_MailL 

ORDER BY #GL_OPEN.CardCode

  End

else

 Begin

   SELECT  #GL_OPEN.* 

       ,'Current' AS [HeaderCurrent]

       ,@Header1 AS Header1

       ,@Header2 AS Header2

       ,@Header3 AS Header3

       ,@Header4 AS Header4

       ,@Header5 AS Header5

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) = 0 THEN 1 

             ELSE DateDiff(Day,DueDate,@DateTo)

        END AS [Days]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) <=0  

               THEN TransAmt 

             ELSE 0

        END AS [Current]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN 1 AND @Bracket1  

               THEN TransAmt 

             ELSE 0

        END AS [Bracket1]     

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN @Bracket1 + 1 AND @Bracket2   

               THEN TransAmt 

             ELSE 0

        END AS [Bracket2]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN @Bracket2 + 1 AND @Bracket3   

               THEN TransAmt 

             ELSE 0

        END AS [Bracket3]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN @Bracket3 + 1 AND @Bracket4   

               THEN TransAmt 

             ELSE 0

        END AS [Bracket4] 

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) >= @Bracket4 + 1 

               THEN TransAmt 

             ELSE 0

        END AS [Bracket5]  

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) <=0  

               THEN TransAmt 

             ELSE 0

        END AS [CurrentFC]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN 1 AND @Bracket1  

               THEN TransAmt 

             ELSE 0

        END AS [Bracket1FC]    

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN @Bracket1 + 1 AND @Bracket2   

               THEN TransAmt 

             ELSE 0

        END AS [Bracket2FC]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN @Bracket2 + 1 AND @Bracket3   

               THEN TransAmt 

             ELSE 0

        END AS [Bracket3FC]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) BETWEEN @Bracket3 + 1 AND @Bracket4   

               THEN TransAmt 

             ELSE 0

        END AS [Bracket4FC]

       ,CASE WHEN DateDiff(Day,DueDate,@DateTo) >= @Bracket4 + 1 

               THEN TransAmt 

             ELSE 0

        END AS [Bracket5FC] 

       ,ISNULL((SELECT XX.PaymentDetails FROM #PAYMENT_DETAILS XX WHERE XX.CardCode = #GL_OPEN.CardCode),'') AS PaidTransaction  

       ,@SysCurr AS SystemCurrency

       ,@LocCurr AS LocalCurrency 

       ,ISNULL(@CompName,'') AS CompanyName

       ,ISNULL(@AliasName,'') AS AliasName

       ,ISNULL(@AddrLine1,'') AS AddressLine1

       ,ISNULL(@AddrLine2,'') AS AddressLine2

       ,ISNULL(@AddrLine3,'') AS AddressLine3

       ,ISNULL(@AddrLine4,'') AS AddressLine4

       ,ISNULL(@AddrLine5,'') AS AddressLine5

       ,ISNULL(@AddrLine6,'') AS AddressLine6

       , @Street AS Street,  

		 @StreetNo AS StreetNo,  

		 @Block AS Block,  

		 @ZipCode AS ZipCode,  

		 @State AS State,  

		 @ComCountry as ComCountry

		 ,@ComEmail AS CompanyEmail

		,d.ContactName

		,d.ContactTel

		,d.ContactFax

		,#GL_OPEN.BillToStreet + ' ' + ISNULL(#GL_OPEN.BillToBlock,'') + ' ' + convert(varchar(100), ISNULL(#GL_OPEN.BillToCity,'' )) + char(10) +  upper(#GL_OPEN.BillToCountry) + ' ' + ISNULL(#GL_OPEN.BillToZipCode,'') as [Bill to]

	  ,@DateTo AS StatementDate

	  ,@CompName AS COMPNAME

	  ,@CompAdd  AS COMPADD

	  ,@CompTel AS COMPTEL

	  ,@CompFax AS COMPFAX

	  ,@CompRegNo AS COMPREGNO

	  ,@GSTRegNo as GSTRegNo

	  ,(select Top 1 LogoImage from OADP) as LogoImage

FROM #GL_OPEN LEFT OUTER JOIN

	#ContactDtls d ON d.CardCode=#GL_OPEN.CardCode

--where series='PY'

--WHERE #GL_OPEN.DocDate <= @DateTo 

--  ORDER BY #GL_OPEN.CardCode, #GL_OPEN.DocDate, #GL_OPEN.TransId, #GL_OPEN.TransType

ORDER BY #GL_OPEN.TransType, #GL_OPEN.DocNum

 End 
DROP TABLE #GLDROP TABLE #GL_OPENDROP TABLE #APPLIED_PAYMENTDROP TABLE #NOT_APPLICABLEDROP TABLE #PAYMENT_DETAILSSET NOCOUNT OFFEND
