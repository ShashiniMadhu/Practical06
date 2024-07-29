object Quec01 {

  case class Product(name: String, quantity: Int, price: Int)

  val inventory1: Map[Int, Product] = Map(
    101 -> Product("TV", 20, 50000),
    102 -> Product("Laptop", 30, 200000),
    103 -> Product("Radio", 5, 10000)
  )

  val inventory2: Map[Int, Product] = Map(
    104 -> Product("Tablet", 10, 12000),
    102 -> Product("Laptop", 5, 200000),
    103 -> Product("Radio", 2, 10000)
  )

  // Calculate the total value of all products in inventory1.
  def calValue1(inventory: Map[Int, Product]): Int = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  //Check whether inventory1 is empty
  def checkEmpty(inventory: Map[Int, Product]): Boolean = {
    val output=inventory.isEmpty
    if(output){
      println("Inventory1 is empty.")
    }
    else{
      println("Inventory1 is not empty")
    }
    output
  }

  // Merge inventory1 and inventory2, updating quantities and retaining the highest price
  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    inventory2.foldLeft(inventory1) { case (acc, (id, newProduct)) =>
      acc.get(id) match {
        case Some(existingProduct) =>
          acc.updated(id, Product(
            existingProduct.name,
            existingProduct.quantity + newProduct.quantity,
            math.max(existingProduct.price, newProduct.price)
          ))
        case None =>
          acc + (id -> newProduct)
      }
    }
  }

  // V. Check if a product with a specific ID (e.g., 102) exists and print its details
  def checkProduct(inventory:Map[Int,Product],Id:Int):Unit={
    if (inventory.contains(Id)) {
      println(s"Product ID $Id Details: ${inventory(Id)}")
    }
    else {
      println(s"Product ID $Id does not exist in inventory1")
  }
}

  def main(args: Array[String]): Unit = {
    println()
    println("Total value of all products in inventory1: " + calValue1(inventory1));

    println()
    checkEmpty(inventory1);

    println()
    val mergedInventory = mergeInventories(inventory1, inventory2);
    println("Merged Inventory: " + mergedInventory);

    println()
    checkProduct(inventory1,102);

  }
}

