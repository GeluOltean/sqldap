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
    // for basic queries the general structure would be
    // SELECT [address] WHERE [condition] AND [condition]

    // SELECT statement: fields are declared in a String array, which is then either used in the search() directly, or
    // provided to a SearchControls object; since using SearchControls means we are unable to use
    // javax.naming.directory.Attributes, we should probably go with directly providing the array to the method

    // WHERE statement: to be translated into parameters for the DirContext.search() method;
    // there are several implementations, including executing string queries; however, using a hashmap-like structure,
    // in the form of javax.naming.directory.Attributes, is perhaps the easiest to parse and debug; if a user already
    // knows the LDAP query language, they wouldn't have a reason to use this DSL in the first place
    // NOTE: Wildcards are empty fields, not asterisks (which are the actual ldap standard)

  }
}
