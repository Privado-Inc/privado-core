/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.languageEngine.java.tagger

import ai.privado.cache.{AppCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.SinkExporter
import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.model.Constants
import ai.privado.model.exporter.SinkModel
import ai.privado.model.exporter.SinkEncoderDecoder.*
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.testfixtures.JavaFrontendTestSuite

import scala.collection.mutable

class JavaS3TaggerTest extends JavaFrontendTestSuite {

  "Java code reading and writing from S3 bucket" should {

    val cpg = code("""
        |import software.amazon.awssdk.core.sync.RequestBody;
        |import software.amazon.awssdk.services.s3.S3Client;
        |import software.amazon.awssdk.services.s3.model.PutObjectRequest;
        |import software.amazon.awssdk.services.s3.model.GetObjectRequest;
        |
        |public class AWS {
        |    public static void main(String[] args) {
        |        S3Client s3Client = S3Client.builder().build();
        |        String objectKey = "your-object-key";
        |        String filePath = "path/to/your/file.txt";
        |
        |        PutObjectRequest request = PutObjectRequest.builder()
        |            .bucket("my-write-bucket")
        |            .key(objectKey)
        |            .build();
        |
        |        GetObjectRequest request2 = GetObjectRequest.builder()
        |                .key(objectKey)
        |                .bucket("my-read-bucket")
        |                .build();
        |
        |        ResponseBytes<GetObjectResponse> objectBytes = s3Client.getObjectAsBytes(request2);
        |            byte[] data = objectBytes.asByteArray();
        |
        |            // Write the data to a local file.
        |            File myFile = new File(filePath);
        |            OutputStream os = new FileOutputStream(myFile);
        |            os.write(data);
        |            System.out.println("Successfully obtained bytes from an S3 object");
        |            os.close();
        |
        |        s3Client.putObject(request, RequestBody.fromFile(new File(filePath));
        |        s3Client.close();
        |    }
        |}
        |""".stripMargin)

    "have bucket name" in {
      val outputMap = cpg.getPrivadoJson()
      val sinks = outputMap(Constants.sinks)
        .as[List[SinkModel]]
        .getOrElse(List())

      sinks.map(_.databaseDetails.dbName) shouldBe List("my-write-bucket", "my-read-bucket")
    }
  }
}
