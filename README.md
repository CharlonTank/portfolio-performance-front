This is the front that comes with portfolio-performance-api

---------------------------------------------------
initial thoughts before coding :

MVP ->

1) Need to get all the different "allocations selected" in the portfolio from the "time selected" to now. With API key stored in backend

2) Calculation : value

2) Show all the assets and how much difference ( +XXXX or -XXXX)

3) Add mutualization option

4) Add vizualisation of the graphs

--

5) Add rebalance frequency :

total ---> new_total

allocation ---> new_allocation
(x y z -> new_x new_y new_z)

new_total * x = new_x_rebalanced
new_total * y = new_y_rebalanced
new_total * z = new_z_rebalanced

5a)

total_old -> total_now

rebalance? year? month?

list of all intervaled values for each asset


//// This automatically rebalances each allocation
for each element in the list
    new_total = calculate element(total) 
end

new_total


6) Save and retrieve user input

database :

portfolio_state
    total : Integer
    start_date : DateTime
    rebalance_status : Integer (No Monthly Yearly)
    token : String (Uniq)

allocation
    symbol : String
    percentage : Int

portfolio_state has_many allocations

now, users can save their inputs with their generated tokens and it changes its URL.

Once they did it ONE time, they can either save again with the same token or they can save again
with a new token so it's gonna change their URL again

url?token=<token>
-> get portfolio state where token is <token>
-> fill inputs
-> maybe launch the calculation direcly without user inputs

