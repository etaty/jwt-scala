package pdi.jwt

trait KeyToArrayByte[A] { self =>

  def apply(key: A): Array[Byte]

  def contramap[B](f: B => A): KeyToArrayByte[B] = new KeyToArrayByte[B] {
    override def apply(key: B): Array[Byte] = self.apply(f(key))
  }
}


object KeyToArrayByte {
  def apply[T](implicit instance: KeyToArrayByte[T]): KeyToArrayByte[T] = instance

  def instance[A](f: A => Array[Byte]): KeyToArrayByte[A] = new KeyToArrayByte[A] {
    override def apply(key: A): Array[Byte] = f(key)
  }

  implicit val stringKeyToArrayByte: KeyToArrayByte[String] = new KeyToArrayByte[String] {
    override def apply(key: String): Array[Byte] = key.getBytes("UTF-8")
  }

  implicit val arrayByteToArrayByte: KeyToArrayByte[Array[Byte]] = new KeyToArrayByte[Array[Byte]] {
    override def apply(key: Array[Byte]): Array[Byte] = key
  }
}

case class BlockKey(value: String)

object BlockKey {
  implicit val blockKeyToArrayByte = new KeyToArrayByte[BlockKey] {
    override def apply(key: BlockKey): Array[Byte] = {
      JwtBase64.decodeNonSafe(
        key.value.replaceAll("-----BEGIN (.*)-----", "")
          .replaceAll("-----END (.*)-----", "")
          .replaceAll("\r\n", "")
          .replaceAll("\n", "")
          .trim
      )
    }
  }
}