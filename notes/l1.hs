import Test.QuickCheck

dollarRate2018 = 1.18215

dollarRate2019 = 1.3671

dollarRate2022 = 0.98546541

dollarRate = 0.91691801

-- Comment starts like this

-- | convert EUR to USD
usd euros = euros * dollarRate

-- | convert USD to EUR
euro usd = usd / dollarRate

prop_EuroUSD x = euro (usd x) == x

-- Nearly equal
x ~== y = x - y < 10e-15 -- Made a new operator | ~==