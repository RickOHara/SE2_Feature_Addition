
public class shit
{
	static final int LINK_INDEX = 17;
	public static void main(String[] args)
	{
		Uploader up = new Uploader("poop", "Avatarjakeneytiri.bmp");
		String json = up.Upload();
		
		String[] json_a = json.split("[{}\\s*,\"]", 0);
		
		String link = (json_a[LINK_INDEX].replace("\\", "")).replace(".png", "");
		System.out.println(link);
		
		up.openLink(link);
	}
}
