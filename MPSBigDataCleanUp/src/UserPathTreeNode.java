import java.util.ArrayList;

public class UserPathTreeNode {
	private String fid;
	private String uri;
	private String ref;
	private String sttp;
	private String ts;
	private String ip;
	private String ancestors;
	private String mid; 
	private String ti;
	private String ua;
	private String uuid;
	private ArrayList<UserPathTreeNode> children;
	private UserPathTreeNode parent = null;
	
	/**
	 * Input following values in order to construct a new node:
	 * "fid", "ref", "sttp", "uri", "ts", "ip", "ancestors", "mid", "ti", "ua", "uuid"
	**/
	public UserPathTreeNode(String fid, String ref, String sttp, String uri, String ts, String ip,
		String ancestors, String mid, String ti, String ua, String uuid){
		this.fid = fid;
		this.uri = uri;
		this.ref = ref;
		this.sttp = sttp;
		this.ts = ts;
		this.ip = ip;
		this.ancestors = ancestors;
		this.mid = mid;
		this.ti = ti;
		this.ua = ua;
		this.uuid = uuid;
		this.children = new ArrayList<>();
	}
	
	/**
	 * set parent
	 * @param UserPathTreeNode 
	**/
	public void setParent(UserPathTreeNode parentNode){
		this.parent = parentNode;
	}
	
	/**
	 * append child to children list
	 * @param TreeNode child
	 */
	public void appendChild(UserPathTreeNode child){
		children.add(child);
		child.setParent(this);
	}
	
	
}
