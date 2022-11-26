package dev.jtrim777.hcl4s.parser

import cats.effect.{IO, Resource}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.Path
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto._

object Secretive {
  private val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
  private val secretSalt = "dev.jtrim777.hcl4s.parser".getBytes

  def encode(raw: String, key: SecretKey): IO[Array[Byte]] = {
    cipher.init(Cipher.ENCRYPT_MODE, key)

    for {
      baos <- using(new ByteArrayOutputStream())
      cos <- using(new CipherOutputStream(baos, cipher))
      _ <- IO(baos.write(cipher.getIV))
      _ <- IO(cos.write(raw.getBytes))
      rez <- IO(baos.toByteArray)
    } yield rez
  }

  def encodeToFile(raw: String, key: SecretKey, dest: Path): IO[Unit] = {
    cipher.init(Cipher.ENCRYPT_MODE, key)

    for {
      fos <- using(new FileOutputStream(dest.toFile))
      _ <- IO(fos.write(cipher.getIV))
      cos <- using(new CipherOutputStream(fos, cipher))
      _ <- IO(cos.write(raw.getBytes))
    } yield ()
  }

  def decode(encoded: Array[Byte], key: SecretKey): IO[String] = {
    for {
      bais <- using(new ByteArrayInputStream(encoded))
      fiv <- IO(bais.readNBytes(16))
      _ <- IO(cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(fiv)))
      cis <- using(new CipherInputStream(bais, cipher))
      bytes <- IO(LazyList.continually(cis.read).takeWhile(_ != -1).map(_.toByte).toArray)
      text <- IO(new String(bytes))
    } yield text
  }

  def decodeFromFile(source: Path, key: SecretKey): IO[String] = {
    for {
      fis <- using(new FileInputStream(source.toFile))
      fiv <- IO(fis.readNBytes(16))
      _ <- IO(cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(fiv)))
      cis <- using(new CipherInputStream(fis, cipher))
      bytes <- IO(LazyList.continually(cis.read).takeWhile(_ != -1).map(_.toByte).toArray)
      text <- IO(new String(bytes))
    } yield text
  }

  def loadSecretKey(token: String): IO[SecretKey] = IO {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val spec = new PBEKeySpec(token.toCharArray, secretSalt, 65536, 256)
    new SecretKeySpec(factory.generateSecret(spec).getEncoded, "AES")
  }

  private case class UsedResource[T](base: Resource[IO, T]) {
    def flatMap[B](effect: T => IO[B]): IO[B] = base.use(effect)

    def map[B](fx: T => B): IO[B] = base.use(t => IO(fx(t)))
  }

  private def using[T <: java.io.Closeable](creator: => T): UsedResource[T] = {
    UsedResource(Resource.make(IO(creator))(t => IO(t.close())))
  }
}
