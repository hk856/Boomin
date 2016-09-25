import java.util.ArrayList;

public class UserPathTreeNode {
	private String URI;
	private String ref;
	private String sttp;
	private String ts;
	private String ip;
	private ArrayList<UserPathTreeNode> children;
	private String parent;
	
	public UserPathTreeNode(String URI, String ref, String sttp, String ts, String ip){
		this.URI = URI;
		this.ref = ref;
		this.sttp = sttp;
		this.ts = ts;
		this.ip = ip;
	}
	
	public void setParent(String parentPth){
		this.parentPth = parentPth;
	}
	
	public void appendChild(UserPathTreeNode child){
		children.add(child);
		child.setParent(ip);
	}
}
