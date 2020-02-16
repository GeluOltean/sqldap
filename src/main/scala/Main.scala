import java.util.Properties

import scala.jdk.CollectionConverters._
import javax.naming.{Context, NamingEnumeration}
import javax.naming.directory.{BasicAttributes, InitialDirContext, SearchControls, SearchResult}

object Main {
  def main(args: Array[String]): Unit = {
    val ldapSettings: Properties = new Properties()
    ldapSettings.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
    ldapSettings.put(Context.SECURITY_AUTHENTICATION, "simple")
    ldapSettings.put(Context.SECURITY_PRINCIPAL, "cn=read-only-admin,dc=example,dc=com")
    ldapSettings.put(Context.SECURITY_CREDENTIALS, "password")
    ldapSettings.put(Context.PROVIDER_URL, "ldap://ldap.forumsys.com:389")

    val searchControls: SearchControls = new SearchControls()
    searchControls.setReturningAttributes(Array[String]("*"))
    searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE)

    val dirContext: InitialDirContext = new InitialDirContext(ldapSettings)
    val attribs: BasicAttributes = new BasicAttributes()
    val results: Iterator[SearchResult] = dirContext.search("dc=example,dc=com", attribs).asScala
    results.foreach(println(_))
  }
}
