
create PROCEDURE [dbo].[AB_SOA_OS_SP004]
   @BPFrom NVARCHAR(30), 
   @BPTo NVARCHAR(30), 
   @DateTo DATETIME,
   @Condition Numeric(2) 
SET @DateFrom = ''
IF @DateTo IS NULL SET @DateTo = getdate()
DECLARE @SysCurr NVARCHAR(3)
Declare @Street   varchar(50)  
set @Street =  (select Top 1 ADM1.Street from ADM1)  
/* Retrieve Company Info */        
DECLARE  @Interval1 INT, @Interval2 INT
DECLARE  @Header1 NVARCHAR(20), @Header2 NVARCHAR(20)
DECLARE  @Bracket1 INT, @Bracket2 INT
SELECT TOP 1 
SELECT TOP 1 
IF @Interval = 15 
SET @Bracket1 = @Interval1
SET @Header1 = '1 to '+ RTRIM(@Bracket1) + ' Days' 
/* Get Reconciliation Sum base on BP */
/*General Ledger*/
SELECT * 
/*Fully Paid but Unreconciled Invoices and Payments*/
--select '285',* from #APPLIED_PAYMENT
--select '247' as '247', * from #APPLIED_PAYMENT
/*Unknown Transaction*/
SELECT DISTINCT CARDCODE INTO #NOT_APPLICABLE FROM #APPLIED_PAYMENT WHERE InvType NOT IN (13, 14, 30)
/* Apply to AR Invoices */
UPDATE #GL_OPEN 
UPDATE #GL_OPEN 
--select '315', * from #GL_OPEN
/* Apply to AR Credit Note */
UPDATE #GL_OPEN 
/* Apply to Incoming Payments */
--UPDATE #GL_OPEN SET TransAmt = CASE WHEN DocCurrency = @LocCurr THEN LCAmount ELSE FCAmount END
--UPDATE #GL_OPEN SET TransAmt = CASE WHEN DebitFC=0 AND CreditFC=0 THEN LCAmount ELSE FCAmount END
--select '360',SUM(BALANCE), count(*) from #GL_OPEN
--select '363',* from #GL_OPEN
DELETE FROM #GL_OPEN WHERE TransAmt = 0
--select '368',SUM(BALANCE), count(*) from #GL_OPEN
DECLARE  @CardCode NVARCHAR(30)
SELECT @F_RefDate = F_RefDate, @T_RefDate = T_RefDate 
IF @T_RefDate > @DateTo SET @T_RefDate = @DateTo
SELECT CONVERT(NVARCHAR(30),'') AS CardCode
DECLARE Payment CURSOR FOR 
SELECT DISTINCT CardCode 
/* Create a temp table for contact details 
----select  '567',SUM(BALANCE),SUM(LCAmount), count(*) from #GL_OPEN
--select DocCurrency,SUM(TransAmt)
--select 'Summary-oustanding',DocCurrency,SUM(TransAmt), count(*) 
--select CardCode,SUM(BALANCE), count(*) from #GL_OPEN
-- @Condition value 1 - Load the Aging Amount, Card Name, Email Address in the MAtrix
if @condition = 1  
           for XML PATH ('') ), 2,10000) [Email] into EmailTable from crd1 t1 group by t1.cardcode
DROP TABLE #GL
END