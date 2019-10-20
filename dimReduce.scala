import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.mllib.feature.PCA
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.rdd.RDD

object DimReduce {

  def main(args: Array[String]) {

    val conf = new SparkConf()
      .setAppName("Dimensionality Reduction")
      .setMaster("local") // remove this when running in a Spark cluster

    val sc = new SparkContext(conf)

    println("Connected to Spark")

    // Display only ERROR logs in terminal
    sc.setLogLevel("ERROR")

    //if (args.length == 0) {println("i need two parameters ")}

    val data: RDD[LabeledPoint] = sc.parallelize(
      Seq(
        new LabeledPoint(0, Vectors.dense(1, 0, 0, 0, 1)),
        new LabeledPoint(1, Vectors.dense(1, 1, 0, 1, 0)),
        new LabeledPoint(1, Vectors.dense(1, 1, 0, 0, 0)),
        new LabeledPoint(0, Vectors.dense(1, 0, 0, 0, 0)),
        new LabeledPoint(1, Vectors.dense(1, 1, 0, 0, 0))
      ))

    // Compute the top 5 principal components.
    val pca = new PCA(5).fit(data.map(_.features))

    // Project vectors to the linear space spanned by the top 5 principal components, keeping the label
    val projected = data.map(p => p.copy(features = pca.transform(p.features)))

    projected.foreach(println)

    sc.stop()
    println("Disconnected from Spark")

  }

}
