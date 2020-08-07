-- No transaction
BEGIN TRY
	INSERT INTO SalesLT.SalesOrderHeader (DueDate, CustomerID, ShipMethod)
	VALUES
	(DATEADD(dd, 7, GETDATE()), 1, 'STD DELIVERY');

	DECLARE @SalesOrderID int = SCOPE_IDENTITY();

	INSERT INTO SalesLT.SalesOrderDetail (SalesOrderID, OrderQty, ProductID, UnitPrice, UnitPriceDiscount)
	VALUES
	(@SalesOrderID, 1, 99999, 1431.50, 0.00);
END TRY
BEGIN CATCH
	PRINT ERROR_MESSAGE();
END CATCH;

-- View orphaned orders
SELECT h.SalesOrderID, h.DueDate, h.CustomerID, h.ShipMethod, d.SalesOrderDetailID
FROM SalesLT.SalesOrderHeader AS h
LEFT JOIN SalesLT.SalesOrderDetail AS d
ON d.SalesOrderID = h.SalesOrderID
WHERE D.SalesOrderDetailID IS NULL;

-- Manually delete orphaned record
DELETE FROM SalesLT.SalesOrderHeader
WHERE SalesOrderID = SCOPE_IDENTITY();

-- Use a transaction
BEGIN TRY
  BEGIN TRANSACTION
	INSERT INTO SalesLT.SalesOrderHeader (DueDate, CustomerID, ShipMethod)
	VALUES
	(DATEADD(dd, 7, GETDATE()), 1, 'STD DELIVERY');

	DECLARE @SalesOrderID int = SCOPE_IDENTITY();

	INSERT INTO SalesLT.SalesOrderDetail (SalesOrderID, OrderQty, ProductID, UnitPrice, UnitPriceDiscount)
	VALUES
	(@SalesOrderID, 1, 99999, 1431.50, 0.00);
  COMMIT TRANSACTION
END TRY
BEGIN CATCH
  IF @@TRANCOUNT > 0
  BEGIN
    PRINT XACT_STATE();
	ROLLBACK TRANSACTION;
  END
  PRINT ERROR_MESSAGE();
  THROW 50001,'An insert failed. The transaction was cancelled.', 0;
END CATCH;

-- Check for orphaned orders
SELECT h.SalesOrderID, h.DueDate, h.CustomerID, h.ShipMethod, d.SalesOrderDetailID
FROM SalesLT.SalesOrderHeader AS h
LEFT JOIN SalesLT.SalesOrderDetail AS d
ON d.SalesOrderID = h.SalesOrderID
WHERE D.SalesOrderDetailID IS NULL

-- Use XACT_ABORT
SET XACT_ABORT ON;
BEGIN TRY
  BEGIN TRANSACTION
	INSERT INTO SalesLT.SalesOrderHeader (DueDate, CustomerID, ShipMethod)
	VALUES
	(DATEADD(dd, 7, GETDATE()), 1, 'STD DELIVERY');

	DECLARE @SalesOrderID int = SCOPE_IDENTITY();

	INSERT INTO SalesLT.SalesOrderDetail (SalesOrderID, OrderQty, ProductID, UnitPrice, UnitPriceDiscount)
	VALUES
	(@SalesOrderID, 1, 99999, 1431.50, 0.00);
  COMMIT TRANSACTION
END TRY
BEGIN CATCH
  PRINT ERROR_MESSAGE();
  THROW 50001,'An insert failed. The transaction was cancelled.', 0;
END CATCH;
SET XACT_ABORT OFF;

-- Check for orphaned orders
SELECT h.SalesOrderID, h.DueDate, h.CustomerID, h.ShipMethod, d.SalesOrderDetailID
FROM SalesLT.SalesOrderHeader AS h
LEFT JOIN SalesLT.SalesOrderDetail AS d
ON d.SalesOrderID = h.SalesOrderID
WHERE D.SalesOrderDetailID IS NULL

DECLARE @OrderID int = 71774
DECLARE @error VARCHAR(30) = 'Order #' + cast(@OrderID as VARCHAR) + ' does not exist';

-- Wrap IF ELSE in a TRY block
BEGIN TRY
  IF NOT EXISTS (SELECT * FROM SalesLT.SalesOrderHeader WHERE SalesOrderID = @OrderID)
  BEGIN
    THROW 50001, @error, 0
  END
  ELSE
  BEGIN
    DELETE FROM SalesLT.SalesOrderDetail WHERE SalesOrderID = @OrderID;
    DELETE FROM SalesLT.SalesOrderHeader WHERE SalesOrderID = @OrderID;
  END
END TRY
-- Add a CATCH block to print out the error
BEGIN CATCH
  PRINT ERROR_MESSAGE();
END CATCH