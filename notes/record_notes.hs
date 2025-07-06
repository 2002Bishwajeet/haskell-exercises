-- Additional Notes on Record Data Types
--
-- Records allow you to:
--  1. Automatically generate accessor functions.
--  2. Construct and deconstruct values using named fields.
--  3. Update fields easily with record update syntax.
--
-- Drawbacks:
--  1. Field names are in the global namespace.
--  2. Some debates around performance when updating large records.

-- Example 1: Product record with multiple fields
data Product = Product
  { productName :: String,
    productPrice :: Double,
    productStock :: Int
  }
  deriving (Show)

createProduct :: String -> Double -> Int -> Product
createProduct name price stock = Product name price stock

displayProduct :: Product -> String
displayProduct prod =
  "The product "
    ++ productName prod
    ++ " is priced at $"
    ++ show (productPrice prod)
    ++ " and has "
    ++ show (productStock prod)
    ++ " items in stock."

-- Example 2: Updating a record field using record update syntax
updateStock :: Product -> Int -> Product
updateStock prod newStock = prod {productStock = newStock}

-- Example 3: Using newtype with record syntax for a single field wrapper
-- newtype is used to create a distinct type with no runtime overhead.
newtype Identifier = Identifier {getId :: String}
  deriving (Show)

data Employee = Employee
  { employeeId :: Identifier,
    employeeName :: String,
    employeeRole :: String
  }
  deriving (Show)

createEmployee :: String -> String -> String -> Employee
createEmployee idVal name role = Employee (Identifier idVal) name role

displayEmployee :: Employee -> String
displayEmployee emp =
  "Employee: "
    ++ employeeName emp
    ++ " ["
    ++ getId (employeeId emp)
    ++ "] works as "
    ++ employeeRole emp

-- Example 4: Nested records and updating nested fields
data Address = Address
  { street :: String,
    city :: String,
    zipCode :: Int
  }
  deriving (Show)

data User = User
  { userName :: String,
    userAddress :: Address
  }
  deriving (Show)

updateCity :: User -> String -> User
updateCity usr newCity =
  usr {userAddress = (userAddress usr) {city = newCity}}

-- The code above shows:
--  1. Creating and using records.
--  2. Updating fields using record update syntax.
--  3. Wrapping a field with newtype for added type safety.
