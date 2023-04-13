// Scala ML
// By JJ Reibel
import scala.util.Random
import scala.math.{ceil}
def train_val_test_split(X: Array[Array[Double]], y: Array[Double], val_size: Double = 0.1, test_size: Double = 0.1, epochs: Int = 1, random_state: Option[Long] = None): (Array[Array[Array[Double]]], Array[Array[Array[Double]]], Array[Array[Array[Double]]], Array[Array[Double]], Array[Array[Double]], Array[Array[Double]]) = {
  // Get the total number of samples in the dataset
  val n_samples = X.length
  // Set the random seed if provided
  if (random_state.isDefined) {
    Random.setSeed(random_state.get)
  }
  // Create a list of indices that correspond to the samples in the dataset
  val idx = Range(0, n_samples).toArray
  // Shuffle the indices
  val shuffled_idx = Random.shuffle(idx.toList).toArray
  // Calculate the number of samples to allocate to the validation and test sets
  val n_val = ceil(n_samples * val_size).toInt
  val n_test = ceil(n_samples * test_size).toInt
   // Initialize the starting and ending indices of each epoch
  val epoch_start_idx = (0 until epochs).map(i => i * n_samples / epochs).toArray
  val epoch_end_idx = epoch_start_idx.tail ++ Array(n_samples)
   // Initialize the lists to hold the indices of the samples in each set for each epoch
  var train_idx_epoch = Array.ofDim[Int](epochs, n_samples - n_val - n_test)
  var val_idx_epoch = Array.ofDim[Int](epochs, n_val)
  var test_idx_epoch = Array.ofDim[Int](epochs, n_test)
   // Loop through each epoch
  for (i <- 0 until epochs) {
    // Get the indices of the samples in the current epoch
    val epoch_indices = shuffled_idx.slice(epoch_start_idx(i), epoch_end_idx(i))
       // Allocate samples to the validation and test sets
    val val_indices = epoch_indices.slice(0, n_val)
    val test_indices = epoch_indices.slice(n_val, n_val + n_test)
    val train_indices = epoch_indices.slice(n_val + n_test, epoch_indices.length)
       // Add the indices to the lists for the current epoch
    train_idx_epoch(i) = train_indices
    val_idx_epoch(i) = val_indices
    test_idx_epoch(i) = test_indices
  }
   // Initialize lists to hold the data for each epoch
  var X_train_epoch = Array.ofDim[Double](epochs, n_samples - n_val - n_test, X(0).length)
  var X_val_epoch = Array.ofDim[Double](epochs, n_val, X(0).length)
  var X_test_epoch = Array.ofDim[Double](epochs, n_test, X(0).length)
  var y_train_epoch = Array.ofDim[Double](epochs, n_samples - n_val - n_test)
  var y_val_epoch = Array.ofDim[Double](epochs, n_val)
  var y_test_epoch = Array.ofDim[Double](epochs, n_test)
   // Loop through each epoch
  for (i <- 0 until epochs) {
    // Get the indices of the samples for the current epoch
    val train_idx = train_idx_epoch(i)
    val val_idx = val_idx_epoch(i)
    val test_idx = test_idx_epoch(i)
    // Populate the data lists for the current epoch
    for (j <- 0 until train_idx.length) {
      X_train_epoch(i)(
        j) = X(train_idx(j))
      y_train_epoch(i)(j) = y(train_idx(j))
    }
    for (j <- 0 until val_idx.length) {
      X_val_epoch(i)(
        j) = X(val_idx(j))
      y_val_epoch(i)(j) = y(val_idx(j))
    }
    for (j <- 0 until test_idx.length) {
      X_test_epoch(i)(
        j) = X(test_idx(j))
      y_test_epoch(i)(j) = y(test_idx(j))
    }
  }
  // Return the data for each set for each epoch
  (X_train_epoch, X_val_epoch, X_test_epoch, y_train_epoch, y_val_epoch, y_test_epoch)
}
// Test the function with some sample data
val X = Array(
Array(1.0, 2.0, 3.0, 4.0),
Array(2.0, 3.0, 4.0, 5.0),
Array(3.0, 4.0, 5.0, 6.0),
Array(4.0, 5.0, 6.0, 7.0),
Array(5.0, 6.0, 7.0, 8.0),
Array(6.0, 7.0, 8.0, 9.0),
Array(7.0, 8.0, 9.0, 10.0),
Array(8.0, 9.0, 10.0, 11.0),
Array(9.0, 10.0, 11.0, 12.0),
Array(10.0, 11.0, 12.0, 13.0)
)
val y = Array(1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0)
val (X_train_epoch, X_val_epoch, X_test_epoch, y_train_epoch, y_val_epoch, y_test_epoch) = train_val_test_split(X, y, val_size = 0.2, test_size = 0.2, epochs = 2, random_state = Some(42L))
println("X_train_epoch:")
for (i <- 0 until X_train_epoch.length) {
  println(s"Epoch $i:")
  for (j <- 0 until X_train_epoch(i).length) {
    println(X_train_epoch(i)(j).mkString(","))
  }
}
println("y_train_epoch:")
for (i <- 0 until y_train_epoch.length) {
  println(s"Epoch $i:")
  println(y_train_epoch(i).mkString(","))
}
println("X_val_epoch:")
for (i <- 0 until X_val_epoch.length) {
  println(s"Epoch $i:")
  for (j <- 0 until X_val_epoch(i).length) {
  println(X_val_epoch(i)(j).mkString(","))
  }
}
println("y_val_epoch:")
for (i <- 0 until y_val_epoch.length) {
  println(s"Epoch $i:")
  println(y_val_epoch(i).mkString(","))
}
println("X_test_epoch:")
for (i <- 0 until X_test_epoch.length) {
  println(s"Epoch $i:")
  for (j <- 0 until X_test_epoch(i).length) {
  println(X_test_epoch(i)(j).mkString(","))
  }
}
println("y_test_epoch:")
for (i <- 0 until y_test_epoch.length) {
  println(s"Epoch $i:")
  println(y_test_epoch(i).mkString(","))
}
