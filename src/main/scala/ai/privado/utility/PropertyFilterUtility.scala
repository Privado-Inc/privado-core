package ai.privado.utility

import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty

object PropertyFilterUtility {

  def filter(properties: List[JavaProperty], enableProjectLevel: Boolean = false, enableDirLevel: Boolean = false, enableFileLevel: Boolean = false): List[JavaProperty] = {
   // propertyProjectLevelFiltering(properties)
   properties
  }

  // Here we do filtering from to down approach, first will we do the filtering
  // on project level, after that we will do on Dir level and then at file level
  // if after doing so we didn't get the single instance then will wil show the
  // dataflow for both of them.
  // Following are the Level filtering level explained: 
  // Level 1 (Project level filtering) => check which property file is more
  // closer to call node.
  // Level 2 (Directory level Filtering) => check which property file name is
  // more relevent
  // Level 3 (File level Filtering) => check the value of the property node, does
  // it have relevent data
  // First will we will do the Project Level filtering. If we will get more the
  // two java Property then

//  private def propertyProjectLevelFiltering(properties: List[JavaProperty]): List[JavaProperty] = {
//    val maxMatchedPropertyValue = properties.filter(property => {
//      // logic for max matching string
//    })
//
//    if (maxMatchedPropertyValue.size == 1) {
//      maxMatchedPropertyValue.head
//    } else {
//      PropertyDirLevelSubFiltering(maxMatchedPropertyValue)
//    }
//  }
//
//  private def PropertyDirLevelSubFiltering(properties: List[JavaProperty]): List[JavaProperty] = {
//    if (properties.exists(p => p.file.contains(".*(prod|dev).*"))) {
//
//    } else {
//      PropertyFileLevelFiltering(properties)
//    }
//  }
//
//  private def PropertyFileLevelFiltering(properties: List[JavaProperty]): List[PropertyFilterUtility] = {
//    if (properties.exists(p => p.name.contains(".*(http|https).*"))) {
//
//    } else {
//      properties
//    }
//  }

}
